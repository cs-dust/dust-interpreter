use crate::parser::ast::Literal;

const HEAP_INIT_SIZE:usize = 1024;

const STRING_TYPE:u8 = 0b0000_0000;
const INTEGER_TYPE:u8 = 0b0000_0001;
const BOOL_TYPE:u8 = 0b0000_0010;

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
    fn get_data_payload(data:u128) -> u64 {
        return (data & (u64::MAX as u128)) as u64;
    }
    fn get_node_from_addr(&self, addr:usize) -> u128 {
        return self.heap[addr];
    }

    fn expand_heap(&mut self) -> () {
        for i in 0..self.size {
            self.heap.push(((i + self.size) as u128) << NEXT_NODE_OFFSET);
        }
        self.free_space += self.size;
        self.size *= 2;
        println!("new heap size: {}", self.heap.len());
    }

    pub fn push_string(&mut self, string:String) -> usize {
        while self.free_space <= string.len() + 1 {
            self.expand_heap()
        }
        return 0;
    }
    
    pub fn push_integer(&mut self, integer:u64) -> usize {
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
    pub fn push_boolean(&self, boolean:bool) -> usize {
        return if boolean {0} else {1};
    }
    pub fn get_string(&self, addr:usize) -> String {
        return String::from("Hello");
    }
    pub fn get_integer(&self, addr:usize) -> u64 {
        let header_node = self.get_node_from_addr(addr);
        let data_addr = Heap::get_next_node(header_node);
        let data_node = self.get_node_from_addr(data_addr);
        return Heap::get_data_payload(data_node);
    }
    pub fn get_boolean(&self, addr:usize) -> bool {
        return addr == 0;
    }
    pub fn heap_get(&self, addr:usize) -> Literal {
        return Literal::UnitLiteral;
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
            free_pointer: 2,
            free_space: HEAP_INIT_SIZE,
            size: HEAP_INIT_SIZE,
        };
    }
    pub fn clear_heap(&mut self) {
        self.heap.clear();
        self.free_pointer = 2;
        self.free_space = HEAP_INIT_SIZE - 2;
        self.size = HEAP_INIT_SIZE;
        for i in 0..HEAP_INIT_SIZE - 1 {
            self.heap.push(((i as u128) + 1) << NEXT_NODE_OFFSET);
        }
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
fn check_heap_resize() {
    let mut heap = Heap::new();
    heap.clear_heap();
    for i in 1..HEAP_INIT_SIZE*4 {
        let _ptr = heap.push_integer(i as u64);
    }
    for i in 1..HEAP_INIT_SIZE {
        let value = heap.get_integer(2*i);
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