use std::collections::HashMap;

const HEAP_INIT_SIZE:usize = 1024;

const STRING_TAG:u8 = 0b0000_0001;
const FALSE_TAG:u8 = 0b0000_0010;
const TRUE_TAG:u8 = 0b0000_0011;
const BLOCKFRAME_TAG:u8 = 0b0000_0100;
const CALLFRAME_TAG:u8 = 0b0000_0101;
const FRAME_TAG:u8 = 0b0000_0110;
const ENVIRONMENT_TAG:u8 = 0b0000_0111;

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

pub struct Heap {
    pub heap : Vec<u128>,
    pub map : HashMap<String, u32>,
    pub free_pointer : usize,
}

// TODO: implement dynamic resizing of the heap
impl Heap {
    // private methods
    fn create_node(previous_pointer:u32, next_pointer:u32, payload:u64) -> u128 {
        return (previous_pointer as u128) << PREVIOUS_NODE_OFFSET
                | (next_pointer as u128) << NEXT_NODE_OFFSET
                | (payload as u128) << PAYLOAD_OFFSET;
    }
    fn set_previous(&self, new_previous:u32, index:usize) -> () {
        let old_value:u128 = *self.heap.get(index).expect("Heap::set_previous: Heap index out of bounds");
        let new_value:u128 = old_value
                             | (new_previous as u128) << PREVIOUS_NODE_OFFSET
                               & !(new_previous as u128) << PREVIOUS_NODE_OFFSET;
        self.heap.insert(index, new_value)
    }
    fn set_next(&self, new_next:u32, index:usize) -> () {
        let old_value:u128 = *self.heap.get(index).expect("Heap::set_next: Heap index out of bounds");
        let new_value:u128 = old_value
                             | (new_next as u128) << NEXT_NODE_OFFSET
                               & !(new_next as u128) << NEXT_NODE_OFFSET;
        self.heap.insert(index, new_value)
    }
    fn get_previous(&self, index:usize) -> u32 {
        let node: u128 = *self.heap.get(index).expect("Heap::get_previous: Heap index out of bounds");
        return (node >> PREVIOUS_NODE_OFFSET) as u32;
    }
    fn get_next(&self, index:usize) -> u32 {
        let node: u128 = *self.heap.get(index).expect("Heap::get_next: Heap index out of bounds");
        return (node >> NEXT_NODE_OFFSET) as u32;
    }


    fn initialize(&self) -> () {
        self.heap.insert(0, Heap::create_node(HEAP_INIT_SIZE as u32, 1, 0));
        self.heap.insert(HEAP_INIT_SIZE, Heap::create_node(HEAP_INIT_SIZE as u32 - 1, 0, 0));
        for i in 1..HEAP_INIT_SIZE-1 {
            println!("{}", i);
            self.heap.insert(i, Heap::create_node((i-1) as u32, (i+1) as u32, 0));
        }
    }

    // public methods
    pub fn new() -> Heap {
        let heap = Heap {
            heap : Vec::new(),
            map: HashMap::new(),
            free_pointer: 0
        };
        heap.initialize();
        return heap;
    }

    // TODO: Finish
    // Allocate an array on the heap
    pub fn allocate_array(&self, array:&[u64]) -> u32 {
        let arr_len = array.len();
        let node_before_array = self.get_previous(self.free_pointer);
        return 0;
    }
}