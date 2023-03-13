use std::collections::HashMap;

const HEAP_INIT_SIZE:usize = 1024;

const STRING_TAG:u8 = 0b0000_0001;
const FALSE_TAG:u8 = 0b0000_0010;
const TRUE_TAG:u8 = 0b0000_0011;
const BLOCKFRAME_TAG:u8 = 0b0000_0100;
const CALLFRAME_TAG:u8 = 0b0000_0101;
const FRAME_TAG:u8 = 0b0000_0110;
const ENVIRONMENT_TAG:u8 = 0b0000_0111;

const TAG_OFFSET:u8 = 24;
const DATA_LEN_OFFSET:u8 = 64;
const OWNER_OFFSET:u8 = 32;
const MUT_FLAG_OFFSET:u8 = 23;
const NEXT_BLOCK_OFFSET:u8 = 96;

pub struct Heap {
    pub heap : Vec<u128>,
    pub map : HashMap<String, u32>,
    pub free_pointer : usize,
//    pub initial_pointer : *mut u8
}

impl Heap {
    pub fn new() -> Heap {
        Heap {
            heap : Vec::new(),
            map: HashMap::new(),
            free_pointer: 0
        }
    }
    pub fn init(&self) -> () {
        println!("initializing heap with size {}", HEAP_INIT_SIZE);
        for i in 0..HEAP_INIT_SIZE {
            self.heap.insert(i, (i << NEXT_BLOCK_OFFSET) as u128);
        }
    }
    fn create_header(tag:u8, data_len:u32, next_free:usize, owner:u32, mut_flag:bool) {
        let header:u128 = (next_free as u128) << NEXT_BLOCK_OFFSET
                          | (data_len as u128) << DATA_LEN_OFFSET
                          | (owner as u128) << OWNER_OFFSET
                          | (tag as u128) << TAG_OFFSET
                          | (if mut_flag {1} else {0} as u128) << MUT_FLAG_OFFSET;
    }
    fn get_next_free(&self, address:usize) -> usize {
        return (self.heap.get(address).expect("!?") >> NEXT_BLOCK_OFFSET as u128) as usize;
    }
    pub fn allocate_string(&self, data: String, label: String, owner:u32, mut_flag: bool) -> u32 {
        let num_blocks = data.len() as u32 / 8;
        let str_ptr = self.free_pointer;
        let next_free:usize = self.free_pointer;
        let mut offset_counter = 0;
        let mut data: u64 = 0;
        for char in data.into_bytes() {

        }
        let header = Heap::create_header(STRING_TAG, num_blocks, next_free, owner, mut_flag);
        return 0;
    }

    pub fn get(&self, address:u64) -> u64 {
        return 0;
    }
    pub fn free(&self, address:u64) -> () {

    }
}