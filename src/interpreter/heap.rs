use std::collections::HashMap;

const HEAP_INIT_SIZE:u64 = 1024;

pub struct Heap {
    pub heap : Box<u64>,
    pub map : HashMap<String, u64>,
    pub free_pointer : u64
}

impl Heap {
    pub fn new() -> Heap {
        println!("Allocating {} bytes for heap", HEAP_INIT_SIZE);
        Heap {
            heap : Box::<u64>::new(HEAP_INIT_SIZE),
            map: HashMap::new(),
            free_pointer: 0
        }
    }
    pub fn allocate(&self, size:usize) -> u64 {
        return 0;
    }
    pub fn get(&self, address:usize) -> u64 {
        return 0;
    }
    pub fn free(&self, address:usize) -> () {

    }
}