use std::collections::HashMap;

const HEAP_INIT_SIZE:usize = 1024;

const STRING_TAG:u8 = 0b0000_0001;
const FALSE_TAG:u8 = 0b0000_0010;
const TRUE_TAG:u8 = 0b0000_0011;
const BLOCKFRAME_TAG:u8 = 0b0000_0100;
const CALLFRAME_TAG:u8 = 0b0000_0101;
const FRAME_TAG:u8 = 0b0000_0110;
const ENVIRONMENT_TAG:u8 = 0b0000_0111;
const ARRAY_TAG:u8 = 0b0000_1000;
const VECTOR_TAG:u8 = 0b0000_1001;

/*
 * Heap is implemented as doubly linked list(s)
 * Free memory is one doubly linked list
 * Every item on the heap is a doubly linked list
 *
 * Heap is implemented using a Vec<u128>
 * The leading 64 bits are used to store next and prev pointers (32 bits each)
 * The trailing 64 bits are used to store the data/payload
*/

const PREVIOUS_NODE_OFFSET:u8 = 96;
const NEXT_NODE_OFFSET:u8 = 64;
const PAYLOAD_OFFSET:u8 = 0;

const MUTABLE_FLAG_OFFSET:u8 = 56;
const LENGTH_OFFSET:u8 = 40;
const TYPE_TAG_OFFSET:u8 = 32;
const OWNER_OFFSET:u8 = 0;

const PAYLOAD_MASK:u128 = u64::MAX as u128;
const PREVIOUS_NODE_MASK:u128 = (u32::MAX as u128) << PREVIOUS_NODE_OFFSET;
const NEXT_NODE_MASK:u128 = (u32::MAX as u128) << NEXT_NODE_OFFSET;

pub struct Heap {
    pub heap : Vec<u128>,
    pub map : HashMap<String, u32>,
    pub free_pointer : usize,
    pub free_space : u64,
}

// TODO: implement dynamic resizing of the heap
impl Heap {
    // private methods

    // Create a heap node with the following layout:
    // [127:96 == previous node, 95:64 == next node, 63:0 == payload]
    fn create_node(previous_pointer:usize, next_pointer:usize, payload:u64) -> u128 {
        return (previous_pointer as u128) << PREVIOUS_NODE_OFFSET
                | (next_pointer as u128) << NEXT_NODE_OFFSET
                | (payload as u128) << PAYLOAD_OFFSET;
    }

    // Create a header with the following layout:
    // [64:57 == unused, 56 == mutable flag, 55:40 == length of data structure, 39:32 == type tag, 31:0 == owner]
    fn create_header(previous_pointer:usize, next_pointer:usize, type_tag:u8, owner:u32, mutable:bool, length:u16) -> u128 {
        let payload:u64 = (owner as u64) << OWNER_OFFSET
                            | (type_tag as u64) << TYPE_TAG_OFFSET
                            | (if mutable {1} else {0}) << MUTABLE_FLAG_OFFSET
                            | (length as u64) << LENGTH_OFFSET;
        return Heap::create_node(previous_pointer, next_pointer, payload);
    }

    // Set the previous node pointer of a given node
    fn set_previous(&self, new_previous:usize, index:usize) -> () {
        let old_value:u128 = *self.heap.get(index).expect("Heap::set_previous: Heap index out of bounds");
        let new_value:u128 = old_value
                             | (new_previous as u128) << PREVIOUS_NODE_OFFSET
                               & !(new_previous as u128) << PREVIOUS_NODE_OFFSET;
        self.heap.insert(index, new_value)
    }

    // Set the next node pointer of a given node
    fn set_next(&self, new_next:usize, index:usize) -> () {
        let old_value:u128 = *self.heap.get(index).expect("Heap::set_next: Heap index out of bounds");
        let new_value:u128 = old_value
                             | (new_next as u128) << NEXT_NODE_OFFSET
                               & !(new_next as u128) << NEXT_NODE_OFFSET;
        self.heap.insert(index, new_value)
    }

    // Set the payload of a given node
    fn set_payload(&self, new_payload:u64, index:usize) -> () {
        let old_value:u128 = *self.heap.get(index).expect("Heap::set_payload: Heap index out of bounds");
        let new_value:u128 = old_value
                             | (new_payload as u128) << NEXT_NODE_OFFSET
                               & !(new_payload as u128) << NEXT_NODE_OFFSET;
        self.heap.insert(index, new_value);
    }

    // Set the tag of a given node
    fn set_tag(&self, new_tag:u8, index:usize) -> () {
        let old_payload = self.get_payload(index);
        let new_payload = old_payload
                          | (new_tag as u64) << TYPE_TAG_OFFSET
                            & !(new_tag as u64) << TYPE_TAG_OFFSET;
        self.set_payload(new_payload, index);
    }

    // Get the previous node pointer of a given node
    fn get_previous(&self, index:usize) -> usize {
        let node:u128 = *self.heap.get(index).expect("Heap::get_previous: Heap index out of bounds");
        return (((node & PREVIOUS_NODE_MASK) >> PREVIOUS_NODE_OFFSET) as u32) as usize;
    }

    // Get the next node pointer of a given node
    fn get_next(&self, index:usize) -> usize {
        let node:u128 = *self.heap.get(index).expect("Heap::get_next: Heap index out of bounds");
        return (((node & NEXT_NODE_MASK) >> NEXT_NODE_OFFSET) as u32) as usize;
    }

    // Get the payload of a given node
    fn get_payload(&self, index:usize) -> u64 {
        let payload:u128 = *self.heap.get(index).expect("Heap::get_payload: Heap index out of bounds");
        return ((payload & PAYLOAD_MASK) >> PAYLOAD_OFFSET) as u64;
    }

    // Initialize heap as circular double linked list
    fn initialize(&self) -> () {
        self.heap.insert(0, Heap::create_node(HEAP_INIT_SIZE, 1, 0));
        self.heap.insert(HEAP_INIT_SIZE, Heap::create_node(HEAP_INIT_SIZE - 1, 0, 0));
        for i in 1..HEAP_INIT_SIZE-1 {
            println!("{}", i);
            self.heap.insert(i, Heap::create_node(i-1, i+1, 0));
        }
    }

    // public methods

    // Constructor
    pub fn new() -> Heap {
        let heap = Heap {
            heap : Vec::new(),
            map: HashMap::new(),
            free_pointer: 0,
            free_space: HEAP_INIT_SIZE as u64,
        };
        heap.initialize();
        return heap;
    }

    // Allocate an array on the heap
    pub fn allocate_array(&self, array:&[u64], owner:u32, mutable:bool) -> usize {
        let arr_len = array.len() as u64;
        if self.free_space < arr_len + 1 {
            panic!("Out of memory. Sorry, I haven't implemented heap expansion yet!");
        }
        if arr_len > u16::MAX as u64 {
            panic!("Max supported array length: {} elements. Sorry about that. ", u16::MAX);
        }
        let arr_head_addr = self.free_pointer;
        let arr_addr = self.get_next(arr_head_addr);
        let arr_prev_addr = self.get_previous(arr_head_addr);
        self.free_pointer = self.get_next(self.free_pointer) as usize;
        for element in array {
            self.set_payload(*element, self.free_pointer);
            self.free_pointer = self.get_next(self.free_pointer) as usize;
        }
        self.set_next(self.free_pointer, arr_prev_addr as usize);
        self.set_previous(arr_prev_addr, self.free_pointer);
        let header = Heap::create_header(arr_prev_addr, arr_addr, ARRAY_TAG, owner, mutable, arr_len as u16);
        self.heap.insert(arr_head_addr, header);
        self.free_space -= arr_len + 1;
        return arr_head_addr;
    }

    // Allocate a vector of u64s on the heap
    pub fn allocate_vector_u64(&self, vector:&Vec<u64>, owner:u32, mutable:bool) -> usize {
        let vec_len = vector.len() as u64;
        if self.free_space < vec_len + 1 {
            panic!("Out of memory. Sorry, I haven't implemented heap expansion yet!");
        }
        if vec_len > u16::MAX as u64 {
            panic!("Max supported vector length: {} elements. Sorry about that. ", u16::MAX);
        }
        let vec_head_addr = self.free_pointer;
        let vec_addr = self.get_next(vec_head_addr);
        let vec_prev_addr = self.get_previous(vec_head_addr);
        self.free_pointer = self.get_next(self.free_pointer) as usize;
        for element in vector {
            self.set_payload(*element, self.free_pointer);
            self.free_pointer = self.get_next(self.free_pointer) as usize;
        }
        self.set_next(self.free_pointer, vec_prev_addr as usize);
        self.set_previous(vec_prev_addr, self.free_pointer);
        let header = Heap::create_header(vec_prev_addr, vec_addr, VECTOR_TAG, owner, mutable, vec_len as u16);
        self.heap.insert(vec_head_addr, header);
        self.free_space -= vec_len + 1;
        return vec_head_addr;
    }

    // Allocate a string on the heap
    pub fn allocate_string(&self, string:String, owner:u32, mutable:bool) -> usize {
        let str_len = string.len() as u64 / 8;
        if self.free_space < str_len + 1 {
            panic!("Out of memory. Sorry, I haven't implemented heap expansion yet!");
        }
        if str_len > u16::MAX as u64 {
            panic!("Max supported string length: {} elements. Sorry about that. ", u16::MAX * 8);
        }
        // We will squeeze up to 8 characters into a u64 - magic!
        let str_vec:Vec<u64> = Vec::new();
        let mut str_cnt:u8 = 0;
        let mut to_insert:u64 = 0;
        for char in string.into_bytes() {
            to_insert |= (char as u64) << (8 * str_cnt);
            str_cnt += 1;
            if str_cnt == 8 {
                str_vec.push(to_insert);
                to_insert = 0;
                str_cnt = 0;
            }
        }
        if str_cnt != 0 {
            str_vec.push(to_insert);
        }
        let str_addr = self.allocate_vector_u64(&str_vec, owner, mutable);
        self.set_tag(STRING_TAG, str_addr);
        return str_addr;
    }
}