use crate::parser::ast::Literal;

const HEAP_INIT_SIZE: usize = 1024;

const NUM_LITERAL_TYPES: usize = 4;

const STRING_TYPE: u8 = 0b0000_0000;
const INTEGER_TYPE: u8 = 0b0000_0001;
const FALSE_TYPE: u8 = 0b0000_0010;
const TRUE_TYPE: u8 = 0b0000_0011;
const UNDEFINED_TYPE: u8 = 0b0000_0100;
const MOVED_TYPE: u8 = 0b0000_0101;

const DATA_TYPE_OFFSET: u8 = 0;
const PAYLOAD_OFFSET: u8 = 0;
const LENGTH_OFFSET: u8 = 64;
const NEXT_NODE_OFFSET: u8 = 96;

// Heap data structure
pub struct Heap {
    heap: Vec<u128>,
    free_pointer: usize,
    free_space: usize,
    size: usize,
    debug: bool
}

impl Heap {
    // Private methods
    fn create_header_node(data_type: u8, data_length: u32, next_node: usize) -> u128 {
        let mut header: u128 = 0;
        header |= (data_type as u128) << DATA_TYPE_OFFSET;
        header |= (data_length as u128) << LENGTH_OFFSET;
        header |= ((next_node as u32) as u128) << NEXT_NODE_OFFSET;
        return header;
    }
    fn create_data_node(payload: u64, remaining_data: u32, next_node: usize) -> u128 {
        let mut data_node: u128 = 0;
        data_node |= (payload as u128) << PAYLOAD_OFFSET;
        data_node |= (remaining_data as u128) << LENGTH_OFFSET;
        data_node |= ((next_node as u32) as u128) << NEXT_NODE_OFFSET;
        return data_node;
    }
    fn get_next_node(header: u128) -> usize {
        return ((header >> NEXT_NODE_OFFSET) & u32::MAX as u128) as usize;
    }
    fn get_data_length(node: u128) -> u32 {
        return ((node >> LENGTH_OFFSET) & (u32::MAX as u128)) as u32;
    }
    fn get_data_type(node: u128) -> u8 {
        return ((node >> DATA_TYPE_OFFSET) & (u8::MAX as u128)) as u8;
    }
    fn get_data_payload(data: u128) -> u64 {
        return (data & (u64::MAX as u128)) as u64;
    }
    fn get_node_from_addr(&self, addr: usize) -> u128 {
        return self.heap[addr];
    }

    fn expand_heap(&mut self) -> () {
        for i in 0..self.size {
            self.heap
                .push(((i + self.size + 1) as u128) << NEXT_NODE_OFFSET);
        }
        self.free_space += self.size;
        self.size *= 2;
        if self.debug{
            println!("Expanded heap. ");
            println!("New heap size: {}", self.heap.len());
        }
    }

    fn push_string(&mut self, string: String) -> usize {
        while self.free_space <= string.len() + 1 {
            self.expand_heap()
        }
        let mut curr_ptr = self.free_pointer;
        let header_ptr = curr_ptr;
        let mut curr_node = self.get_node_from_addr(curr_ptr);
        let mut next_ptr = Heap::get_next_node(curr_node);
        let mut remaining_data = string.len();
        let header_node = Heap::create_header_node(STRING_TYPE, remaining_data as u32, next_ptr);
        self.heap[curr_ptr] = header_node;
        curr_ptr = next_ptr;
        remaining_data -= 1;
        for c in string.as_bytes() {
            curr_node = self.get_node_from_addr(curr_ptr);
            next_ptr = Heap::get_next_node(curr_node);
            let new_node = Heap::create_data_node(*c as u64, remaining_data as u32, next_ptr);
            self.heap[curr_ptr] = new_node;
            curr_ptr = next_ptr;
            if remaining_data > 0 {
                remaining_data -= 1;
            } else {
                break;
            }
        }
        self.free_pointer = curr_ptr;
        self.free_space -= string.len() + 1;
        if self.debug {
            println!("Pushed string {} to address {}", &string, header_ptr);
        }
        return header_ptr;
    }

    fn push_integer(&mut self, integer: u64) -> usize {
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

        if self.debug {
            println!("Pushed integer {} to address {}", integer, first_node_addr);
        }

        return first_node_addr;
    }
    fn push_boolean(&self, boolean: bool) -> usize {
        if self.debug {
            println!("Pushed boolean {}", boolean);
        }
        return if boolean { 1 } else { 0 };
    }
    fn get_string(&self, addr: usize) -> String {
        let header_node = self.get_node_from_addr(addr);
        let mut next_ptr = Heap::get_next_node(header_node);
        let remaining_data = Heap::get_data_length(header_node);
        let mut string = String::from("");
        for _i in 0..remaining_data {
            let data_node = self.get_node_from_addr(next_ptr);
            let payload = Heap::get_data_payload(data_node) as u8 as char;
            string.push(payload);
            next_ptr = Heap::get_next_node(data_node);
        }
        return string;
    }

    fn get_integer(&self, addr: usize) -> u64 {
        let header_node = self.get_node_from_addr(addr);
        let data_addr = Heap::get_next_node(header_node);
        let data_node = self.get_node_from_addr(data_addr);
        return Heap::get_data_payload(data_node);
    }
    pub fn heap_push(&mut self, literal: Literal) -> usize {
        return match literal {
            Literal::StringLiteral(string) => {
                self.push_string(string)
            },
            Literal::IntLiteral(integer) => {
                self.push_integer(integer as u64)
            },
            Literal::BoolLiteral(boolean) => {
                self.push_boolean(boolean)
            },
            Literal::UnitLiteral => 2,
            Literal::MovedLiteral => 3,
            _ => panic!()
        };
    }
    pub fn heap_get(&self, addr: usize) -> Literal {
        let node = self.get_node_from_addr(addr);
        let data_type = Heap::get_data_type(node);
        return match data_type {
            STRING_TYPE => Literal::StringLiteral(self.get_string(addr)),
            INTEGER_TYPE => Literal::IntLiteral(self.get_integer(addr) as i64),
            FALSE_TYPE => Literal::BoolLiteral(false),
            TRUE_TYPE => Literal::BoolLiteral(true),
            MOVED_TYPE => Literal::MovedLiteral,
            UNDEFINED_TYPE => Literal::UnitLiteral,
            _ => panic!("Invalid data type"),
        };
    }

    pub fn heap_string_concat(&mut self, addr_a: usize, addr_b: usize) -> usize {
        let node_a_header = self.get_node_from_addr(addr_a);
        let str_a_len = Heap::get_data_length(node_a_header);
        let mut node_a_next = Heap::get_next_node(node_a_header);
        let node_b_header = self.get_node_from_addr(addr_b);
        let str_b_len = Heap::get_data_length(node_b_header);
        while self.free_space < str_b_len as usize {
            self.expand_heap();
        }
        let mut new_str_len = str_a_len + str_b_len;
        let new_header = Heap::create_header_node(STRING_TYPE, new_str_len, node_a_next);
        self.heap[addr_a] = new_header;

        for i in 0..str_a_len {
            let curr_node = self.get_node_from_addr(node_a_next);
            let curr_payload = Heap::get_data_payload(curr_node);
            let next_ptr = Heap::get_next_node(curr_node);
            let new_node = Heap::create_data_node(curr_payload, new_str_len, if i < str_a_len - 1 {next_ptr} else {self.free_pointer});
            self.heap[node_a_next] = new_node;
            node_a_next = next_ptr;
            new_str_len -= 1;
        }

        let mut node_b_next = Heap::get_next_node(node_b_header);
        for _ in 0..str_b_len {
            let node_at_a = self.get_node_from_addr(self.free_pointer);
            let node_at_b = self.get_node_from_addr(node_b_next);
            let b_payload = Heap::get_data_payload(node_at_b);
            node_b_next = Heap::get_next_node(node_at_b);
            let tmp = Heap::get_next_node(node_at_a);
            let new_node = Heap::create_data_node(b_payload, new_str_len, tmp);
            self.heap[self.free_pointer] = new_node;
            self.free_pointer = tmp;
            if new_str_len == 0 {
                break;
            }
            new_str_len -= 1;
        }
        self.free_space -= str_b_len as usize + 1;
        if self.debug {
            println!("Concatenated address {} with address {}", addr_a, addr_b);
        }
        return addr_a;
    }

    pub fn free_space(&mut self, addr: usize) {
        if addr < NUM_LITERAL_TYPES {
            return;
        }
        let node_at_addr = self.get_node_from_addr(addr);
        let data_length = Heap::get_data_length(node_at_addr);
        let mut ptr = addr;
        for _ in 0..data_length {
            let node = self.get_node_from_addr(ptr);
            ptr = Heap::get_next_node(node);
        }
        let new_node = Heap::create_data_node(0, 0, self.free_pointer);
        self.heap[ptr] = new_node;
        self.free_space += data_length as usize + 1;
        self.free_pointer = addr;
        if self.debug {
            println!("Freeing {} nodes at address {}", data_length + 1, addr);
        }
    }
    pub fn new(debug_or_not: bool) -> Heap {
        return Heap {
            heap: Vec::new(),
            free_pointer: NUM_LITERAL_TYPES,
            free_space: HEAP_INIT_SIZE,
            size: HEAP_INIT_SIZE,
            debug: debug_or_not
        };
    }
    pub fn print_stats(&self) {
        println!("Free space: {}", self.free_space);
        println!("Total size: {}", self.size);
        println!("Free pointer: {}", self.free_pointer);
    }
    pub fn clear_heap(&mut self) {
        self.heap.clear();
        self.free_pointer = NUM_LITERAL_TYPES;
        self.free_space = HEAP_INIT_SIZE - NUM_LITERAL_TYPES;
        self.size = HEAP_INIT_SIZE;
        self.heap.push(Heap::create_header_node(FALSE_TYPE, 0, 1));
        self.heap.push(Heap::create_header_node(TRUE_TYPE, 0, 2));
        self.heap
            .push(Heap::create_header_node(UNDEFINED_TYPE, 0, 3));
        self.heap.push(Heap::create_header_node(MOVED_TYPE, 0, 4));
        for i in 0..HEAP_INIT_SIZE - NUM_LITERAL_TYPES {
            self.heap
                .push(((i as u128) + (NUM_LITERAL_TYPES as u128) + 1) << NEXT_NODE_OFFSET);
        }
        // for i in 0..HEAP_INIT_SIZE {
        //     println!("{}", Heap::get_next_node(self.get_node_from_addr(i)));
        // }
    }
}

#[test]
fn check_heap_integer() {
    let mut heap = Heap::new(false);
    heap.clear_heap();
    let ptr = heap.push_integer(35);
    let value = heap.get_integer(ptr);
    assert_eq!(value, 35);
}

#[test]
fn check_heap_bool() {
    let mut heap = Heap::new(false);
    heap.clear_heap();
    let ptr = heap.heap_push(Literal::BoolLiteral(true));
    let value = heap.heap_get(ptr);
    match value {
        Literal::BoolLiteral(b) => {
            assert!(b);
        },
        _ => {
            panic!();
        }
    }
}

#[test]
fn check_push() {
    let mut heap = Heap::new(false);
    heap.clear_heap();
    let int_ptr_1 = heap.heap_push(Literal::IntLiteral(69));
    let bool_ptr_1 = heap.heap_push(Literal::BoolLiteral(false));
    let int_ptr_2 = heap.heap_push(Literal::IntLiteral(420));
    let bool_ptr_2 = heap.heap_push(Literal::BoolLiteral(true));
    let undefined_ptr = heap.heap_push(Literal::UnitLiteral);
    let string_ptr = heap.heap_push(Literal::StringLiteral(String::from("Testing")));
    let _int_1 = match heap.heap_get(int_ptr_1) {
        Literal::IntLiteral(i) => assert_eq!(i, 69),
        _ => panic!(),
    };
    let _int_2 = match heap.heap_get(int_ptr_2) {
        Literal::IntLiteral(i) => assert_eq!(i, 420),
        _ => panic!(),
    };
    let _bool_1 = match heap.heap_get(bool_ptr_1) {
        Literal::BoolLiteral(b) => assert!(!b),
        _ => panic!(),
    };
    let _bool_2 = match heap.heap_get(bool_ptr_2) {
        Literal::BoolLiteral(b) => assert!(b),
        _ => panic!(),
    };
    let _string_2 = match heap.heap_get(string_ptr) {
        Literal::StringLiteral(s) => assert_eq!(s, "Testing"),
        _ => panic!(),
    };
    let _undefined = match heap.heap_get(undefined_ptr) {
        Literal::UnitLiteral => assert!(true),
        _ => panic!(),
    };
}

#[test]
fn check_heap_resize() {
    let mut heap = Heap::new(false);
    heap.clear_heap();
    for i in 1..HEAP_INIT_SIZE * 4 {
        let _ = heap.push_integer(i as u64);
    }
    for i in 1..HEAP_INIT_SIZE * 4 {
        let value = heap.get_integer(2 * i + NUM_LITERAL_TYPES - 2);
        assert_eq!(value, (i as u64));
    }
}

#[test]
fn check_concat() {
    let mut heap = Heap::new(false);
    heap.clear_heap();
    let string_a_ptr = heap.push_string(String::from("cyka"));
    let string_b_ptr = heap.push_string(String::from(" blyat"));
    let string_c_ptr = heap.heap_string_concat(string_a_ptr, string_b_ptr);
    println!("{} {} {}", string_a_ptr, string_b_ptr, string_c_ptr);
    let _string_c = match heap.heap_get(string_c_ptr){
        Literal::StringLiteral(s) => {
            assert_eq!(s, "cyka blyat");
        }
        _ => {
            panic!();
        }
    };
}

#[test]
fn check_free() {
    let mut heap = Heap::new(false);
    heap.clear_heap();
    let ptr1 = heap.push_integer(10);
    let _ = heap.push_integer(20);
    heap.free_space(ptr1);
    let ptr3 = heap.push_integer(30);
    let val1 = heap.get_integer(ptr1);
    let val2 = heap.get_integer(ptr3);
    assert_eq!(val1, val2);
}
