use std::collections::HashMap;
use crate::parser::ast::Literal;

const HEAP_INIT_SIZE:usize = 1024;

const STRING_TYPE:u32 = 0x0000_0000;
const INTEGER_TYPE:u32 = 0x0000_0001;
const BOOL_TYPE:u32 = 0x0000_0002;
const UNIT_LITERAL_TYPE:u32 = 0x0000_0003;
const UNDEFINED_LITERAL_TYPE:u32 = 0x0000_0004;

const PAYLOAD_TAG:u8 = 0b0000_0000;
const STRING_HEADER_TAG:u8 = 0b0000_0001;
const ENVIRONMENT_HEADER_TAG:u8 = 0b0000_0010;
const ARRAY_HEADER_TAG:u8 = 0b0000_0011;
const VECTOR_HEADER_TAG:u8 = 0b0000_0100;

const NEXT_NODE_OFFSET:u8 = 96;
const TAG_OFFSET:u8 = 64;
const METADATA_OFFSET: u8 = 64;
const PAYLOAD_OFFSET:u8 = 0;

const MUTABLE_FLAG_OFFSET:u8 = 56;
const LENGTH_OFFSET:u8 = 40;
const TYPE_TAG_OFFSET:u8 = 32;
const OWNER_OFFSET:u8 = 0;

const ENV_VAR_TYPE_OFFSET:u8 = 32;
const ENV_VAR_ID_OFFSET:u8 = 0;

const PAYLOAD_MASK:u128 = u64::MAX as u128;
const METADATA_MASK:u128 = (u64::MAX as u128) << METADATA_OFFSET;
const NEXT_NODE_MASK:u128 = (u32::MAX as u128) << NEXT_NODE_OFFSET;
const TAG_MASK:u128 = (u8::MAX as u128) << TAG_OFFSET;

const ENV_VAR_TYPE_MASK:u128 = (u32::MAX as u128) << ENV_VAR_TYPE_OFFSET;
const ENV_VAR_ID_MASK:u128 = (u32::MAX as u128) << ENV_VAR_ID_OFFSET;

const LENGTH_MASK:u64 = (u16::MAX as u64) << LENGTH_OFFSET;

pub struct Heap {
    pub heap : Vec<u128>,
    pub map : HashMap<String, u32>,
    pub free_pointer : u32,
    pub free_space : u64,
}


impl Heap {
    // Private methods
    fn get_node (&self, addr:u32) -> u128 {
        return *self.heap.get(addr as usize).expect("Heap index out of bounds");
    }
    fn get_payload (node:u128) -> u64 {
        return ((node & PAYLOAD_MASK) >> PAYLOAD_OFFSET) as u64;
    }
    fn get_metadata (node:u128) -> u64 {
        return ((node & METADATA_MASK) >> METADATA_OFFSET) as u64;
    }
    fn get_tag (node:u128) -> u8 {
        return ((node >> (TAG_OFFSET)) & TAG_MASK) as u8;
    }
    fn get_next_pointer (node:u128) -> u32 {
        return ((node & NEXT_NODE_MASK) >> (NEXT_NODE_OFFSET)) as u32;
    }
    fn get_env_var_type (node:u128) -> u32 {
        return ((node & ENV_VAR_TYPE_MASK) >> (ENV_VAR_TYPE_OFFSET)) as u32;
    }
    fn get_env_var_id (node:u128) -> u32 {
        return ((node & ENV_VAR_ID_MASK) >> (ENV_VAR_ID_OFFSET)) as u32;
    }
    fn create_node (metadata:u64, payload:u64) -> u128 {
        return ((metadata as u128) << METADATA_OFFSET) | ((payload as u128) << PAYLOAD_OFFSET);
    }
    fn create_metadata (next_pointer:u32, tag:u8) -> u64 {
        return ((next_pointer as u64) << (NEXT_NODE_OFFSET - METADATA_OFFSET))
               | ((tag as u64) << (TAG_OFFSET - METADATA_OFFSET));
    }

    // Public methods
    pub fn push_environment (&self, env:&Vec<Literal>) -> u32 {
        let env_size = env.len() as u64;
        let address = self.free_pointer;
        let current_free_node = self.get_node(address);
        let next_free_node_addr = Heap::get_next_pointer(current_free_node);
        let header_metadata = Heap::create_metadata(next_free_node_addr, ENVIRONMENT_HEADER_TAG);
        let header_node = Heap::create_node(header_metadata, env_size);
        return address;
    }
    pub fn pop_environment (&self, addr:u32) -> Vec<Literal> {
        let node_at_addr = self.get_node(addr);
        if Heap::get_tag(node_at_addr) != ENVIRONMENT_HEADER_TAG {
            panic!("Not an environment at this address! How can dis b allow?");
        }
        let env_size = Heap::get_payload(node_at_addr);
        let mut next_pointer = Heap::get_next_pointer(node_at_addr);
        let env_to_return:Vec<Literal> = Vec::new();
        for i in 0..env_size {
            let mut next_node = self.get_node(next_pointer);
            if Heap::get_tag(next_node) != PAYLOAD_TAG {
                panic!("Environment shorter than expected!");
            }
            let var_type = Heap::get_env_var_type(next_node);
            let var_id = Heap::get_env_var_id(next_node);
            next_pointer = Heap::get_next_pointer(next_node);
            next_node = self.get_node(next_pointer);
            if Heap::get_tag(next_node) != PAYLOAD_TAG {
                panic!("Environment shorter than expected!");
            }
            let value = Heap::get_payload(next_node);
            match var_type {
                STRING_TYPE => {
                    // TODO: yell at vishruti and ask why tf stringliteral takes a string i dh string sia
                    // let literal_value = Literal::StringLiteral(value);
                },
                BOOL_TYPE => {
                    let literal_value = Literal::BoolLiteral(if value == 1 {true} else {false});
                },
                INTEGER_TYPE => {
                    let literal_value = Literal::IntLiteral(value as i64);
                },
                UNIT_LITERAL_TYPE => {
                    let literal_value = Literal::UnitLiteral;
                },
                UNDEFINED_LITERAL_TYPE => {
                    let literal_value = Literal::UndefinedLiteral;
                },
                _ => {
                    panic!("Eh sia lah what type is this ah");
                }
            }
            next_pointer = Heap::get_next_pointer(next_node);
        }
        return env_to_return;
    }
    pub fn add_string (&self, string:String) -> u64 {
        return 0;
    }
    pub fn load_string (&self, addr:u32) -> String {
        return String::new();
    }
    pub fn delete_string (&self, addr:u32) -> () {
        return;
    }
    pub fn add_array (&self, arr:&[u64]) -> u64 {
        return 0;
    }
    pub fn load_array (&self, addr:u32) -> &[u64] {
        return &[];
    }
    pub fn delete_array (&self, addr:u32) -> () {
        return;
    }
}