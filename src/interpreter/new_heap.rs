use crate::parser::ast::Literal;
use crate::parser::ast::Literal::{BoolLiteral, UnitLiteral, IntLiteral, StringLiteral};

const HEAP_INIT_SIZE:usize = 1024;

const NUM_LITERAL_TYPES:usize = 3;

const STRING_TYPE:u8 = 0b0000_0000;
const INTEGER_TYPE:u8 = 0b0000_0001;
const FALSE_TYPE:u8 = 0b0000_0010;
const TRUE_TYPE:u8 = 0b0000_0011;
const UNDEFINED_TYPE:u8 = 0b0000_0100;

const DATA_TYPE_OFFSET:u8 = 0;
const PAYLOAD_OFFSET:u8 = 0;
const LENGTH_OFFSET:u8 = 64;
const NEXT_NODE_OFFSET:u8 = 96;

pub struct Heap {
    heap: Vec<u128>,
    free_pointer: usize,
    free_space: usize,
    size: usize,
}

impl Heap {
    fn create_header_node(data_type:u8, data_length:u32, next_node:usize) -> u128 {
        let mut header:u128 = 0;
        header |= (data_type as u128) << DATA_TYPE_OFFSET;
        header |= (data_length as u128) << LENGTH_OFFSET;
        header |= ((next_node as u32) as u128) << NEXT_NODE_OFFSET;
        return header;
    }
    fn create_data_node(payload:u64, remaining_data:u32, next_node:usize) -> u128 {
        let mut data_node:u128 = 0;
        data_node |= (payload as u128) << PAYLOAD_OFFSET;
        data_node |= (remaining_data as u128) << LENGTH_OFFSET;
        data_node |= ((next_node as u32) as u128) << NEXT_NODE_OFFSET;
        return data_node;
    }
    fn get_next_node(header:u128) -> usize {
        return ((header >> NEXT_NODE_OFFSET) & u32::MAX as u128) as usize;
    }
    fn get_data_length(node:u128) -> u32 {
        return ((node >> LENGTH_OFFSET) & (u32::MAX as u128)) as u32;
    }
    fn get_data_type(node:u128) -> u8 {
        return ((node >> DATA_TYPE_OFFSET) & (u8::MAX as u128)) as u8;
    }
    fn get_data_payload(data:u128) -> u64 {
        return (data & (u64::MAX as u128)) as u64;
    }
    fn get_node_from_addr(&self, addr:usize) -> u128 {
        return self.heap[addr];
    }

    fn expand_heap(&mut self) -> () {
        for i in 0..self.size {
            self.heap.push(((i + self.size + 1) as u128) << NEXT_NODE_OFFSET);
        }
        self.free_space += self.size;
        self.size *= 2;
        println!("new heap size: {}", self.heap.len());
    }

    fn push_string(&mut self, string:String) -> usize {
        while self.free_space <= string.len() + 1 {
            self.expand_heap()
        }
        return 0;
    }
    
    fn push_integer(&mut self, integer:u64) -> usize {
        if self.free_space <= 2 {
            self.expand_heap();
        }
        let first_node_addr = self.free_pointer;
        let first_node_current = self.get_node_from_addr(self.free_pointer);
        let second_node_addr = Heap::get_next_node(first_node_current);
        let second_node_current = self.get_node_from_addr(second_node_addr);
        let new_free_pointer = Heap::get_next_node(second_node_current);

        let header_node = Heap::create_header_node(INTEGER_TYPE, 1, second_node_addr);
        let data_node = Heap::create_data_node(integer, 0, new_free_pointer);
        self.heap[first_node_addr] = header_node;
        self.heap[second_node_addr] = data_node;
        self.free_pointer = new_free_pointer;
        self.free_space -= 2;

        return first_node_addr;
    }
    fn push_boolean(&self, boolean:bool) -> usize {
        return if boolean {1} else {0};
    }
    fn get_string(&self, addr:usize) -> String {
        return String::from("Hi");
    }
    fn get_integer(&self, addr:usize) -> u64 {
        let header_node = self.get_node_from_addr(addr);
        let data_addr = Heap::get_next_node(header_node);
        let data_node = self.get_node_from_addr(data_addr);
        return Heap::get_data_payload(data_node);
    }
    fn get_boolean(&self, addr:usize) -> bool {
        return addr == 1;
    }
    pub fn heap_push(&mut self, literal:Literal) -> usize {
        return match literal {
            Literal::StringLiteral(string) => self.push_string(string),
            Literal::IntLiteral(integer) => self.push_integer(integer as u64),
            Literal::BoolLiteral(boolean) => self.push_boolean(boolean),
            Literal::UnitLiteral => 2,
        };
    }
    pub fn heap_get(&self, addr:usize) -> Literal {
        let node = self.get_node_from_addr(addr);
        let data_type = Heap::get_data_type(node);
        return match data_type {
            STRING_TYPE => Literal::StringLiteral(self.get_string(addr)),
            INTEGER_TYPE => Literal::IntLiteral(self.get_integer(addr) as i64),
            FALSE_TYPE => Literal::BoolLiteral(false),
            TRUE_TYPE => Literal::BoolLiteral(true),
            UNDEFINED_TYPE => Literal::UnitLiteral,
            _ => panic!("Invalid data type"),
        };
    }
    pub fn free_space(&mut self, addr:usize)  {
        let node_at_addr = self.get_node_from_addr(addr);
        let data_length = Heap::get_data_length(node_at_addr);
        let mut ptr = addr;
        for i in 0..data_length {
            let node = self.get_node_from_addr(ptr);
            ptr = Heap::get_next_node(node);
        }
        let new_node = Heap::create_data_node(0, 0, self.free_pointer);
        self.heap[ptr] = new_node;
        self.free_pointer = addr;
    }
    pub fn new() -> Heap {
        return Heap {
            heap: Vec::new(),
            free_pointer: NUM_LITERAL_TYPES,
            free_space: HEAP_INIT_SIZE,
            size: HEAP_INIT_SIZE,
        };
    }
    pub fn clear_heap(&mut self) {
        self.heap.clear();
        self.free_pointer = NUM_LITERAL_TYPES;
        self.free_space = HEAP_INIT_SIZE - NUM_LITERAL_TYPES;
        self.size = HEAP_INIT_SIZE;
        self.heap.push(Heap::create_header_node(FALSE_TYPE, 0, 1));
        self.heap.push(Heap::create_header_node(TRUE_TYPE, 0, 2));
        self.heap.push(Heap::create_header_node(UNDEFINED_TYPE, 0, 3));
        for i in 0..HEAP_INIT_SIZE-NUM_LITERAL_TYPES {
            self.heap.push(((i as u128) + (NUM_LITERAL_TYPES as u128) + 1) << NEXT_NODE_OFFSET);
        }
        // for i in 0..HEAP_INIT_SIZE {
        //     println!("{}", Heap::get_next_node(self.get_node_from_addr(i)));
        // }
    }
}

#[test]
fn check_heap_integer() {
    let mut H = Heap::new();
    H.clear_heap();
    let ptr = H.push_integer(35);
    let value = H.get_integer(ptr);
    assert_eq!(value, 35);
}

#[test]
fn check_heap_bool() {
    let mut H = Heap::new();
    H.clear_heap();
    let ptr = H.push_boolean(true);
    let value = H.get_boolean(ptr);
    assert_eq!(value, true);
}

#[test]
fn check_push() {
    let mut heap = Heap::new();
    heap.clear_heap();
    let int_ptr_1 = heap.heap_push(Literal::IntLiteral(69));
    let bool_ptr_1 = heap.heap_push(Literal::BoolLiteral(false));
    let int_ptr_2 = heap.heap_push(Literal::IntLiteral(420));
    let bool_ptr_2 = heap.heap_push(Literal::BoolLiteral(true));
    let undefined_ptr = heap.heap_push(Literal::UnitLiteral);
    let _int_1 = match heap.heap_get(int_ptr_1) {
        IntLiteral(i) => assert_eq!(i, 69),
        _ => panic!()
    };
    let _int_2 = match heap.heap_get(int_ptr_2) {
        IntLiteral(i) => assert_eq!(i, 420),
        _ => panic!()
    };
    let _bool_1 = match heap.heap_get(bool_ptr_1) {
        BoolLiteral(b) => assert!(!b),
        _ => panic!()
    };
    let _bool_2 = match heap.heap_get(bool_ptr_2) {
        BoolLiteral(b) => assert!(b),
        _ => panic!()
    };
    let _undefined = match heap.heap_get(undefined_ptr) {
        UnitLiteral => assert!(true),
        _ => panic!()
    };
}

#[test]
fn check_heap_resize() {
    let mut heap = Heap::new();
    heap.clear_heap();
    for i in 1..HEAP_INIT_SIZE*4 {
        let ptr = heap.push_integer(i as u64);
    }
    for i in 1..HEAP_INIT_SIZE*4 {
        let value = heap.get_integer(2*i+1);
        assert_eq!(value, (i as u64));
    }
}

#[test]
fn check_free() {
    let mut heap = Heap::new();
    heap.clear_heap();
    let ptr1 = heap.push_integer(10);
    let ptr2 = heap.push_integer(20);
    heap.free_space(ptr1);
    let ptr3 = heap.push_integer(30);
    let val1 = heap.get_integer(ptr1);
    let val2 = heap.get_integer(ptr3);
    assert_eq!(val1, val2);
}