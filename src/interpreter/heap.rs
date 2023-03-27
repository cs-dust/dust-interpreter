use std::collections::HashMap;
use crate::parser::ast::Literal;

const HEAP_INIT_SIZE:usize = 1024;

const PAYLOAD_TAG:u8 = 0b0000_0000;
const STRING_HEADER_TAG:u8 = 0b0000_0001;
const ENVIRONMENT_TAG:u8 = 0b0000_0010;
const ARRAY_TAG:u8 = 0b0000_0011;
const VECTOR_TAG:u8 = 0b0000_0100;

/*
 * Heap is implemented as linked list(s)
 * Free memory is one linked list
 * Every item on the heap is a linked list
 *
 * Heap is implemented using a Vec<u128>
 * The leading 32 bits are used to store next pointer (32 bits)
 * Next 32 bits are used to store metadata (
 * The trailing 64 bits are used to store the data/payload
*/

const NEXT_NODE_OFFSET:u8 = 96;
const TAG_OFFSET:u8 = 64;
const PAYLOAD_OFFSET:u8 = 0;

const MUTABLE_FLAG_OFFSET:u8 = 56;
const LENGTH_OFFSET:u8 = 40;
const TYPE_TAG_OFFSET:u8 = 32;
const OWNER_OFFSET:u8 = 0;

const PAYLOAD_MASK:u128 = u64::MAX as u128;
const NEXT_NODE_MASK:u128 = (u32::MAX as u128) << NEXT_NODE_OFFSET;

const LENGTH_MASK:u64 = (u16::MAX as u64) << LENGTH_OFFSET;

pub struct Heap {
    pub heap : Vec<u128>,
    pub map : HashMap<String, u32>,
    pub free_pointer : usize,
    pub free_space : u64,
}


impl Heap {
    pub fn push_environment (&self, env:&Vec<Literal>) -> u64 {
        return 0;
    }
    pub fn pop_environment (&self, addr:u64) -> Vec<Literal> {
        return Vec::new();
    }
    pub fn add_string (&self, string:String) -> u64 {
        return 0;
    }
    pub fn load_string (&self, addr:u64) -> String {
        return String::new();
    }
    pub fn delete_string (&self, addr:u64) -> () {
        return;
    }
    pub fn add_array (&self, arr:&[u64]) -> u64 {
        return 0;
    }
    pub fn load_array (&self, addr:u64) -> &[u64] {
        return &[];
    }
    pub fn delete_array (&self, addr:u64) -> () {
        return;
    }
}