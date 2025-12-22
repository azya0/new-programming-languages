use std::thread;
use std::time::Duration;
use std::collections::{HashMap, VecDeque};
use std::hash::Hash;
use std::sync::Mutex;

struct Cache<K, V> {
    map: HashMap<K, (V, usize)>,
    order: VecDeque<K>,
    capacity: usize,
}

pub struct ThreadSafeLRUCache<K, V> {
    cache: Mutex<Cache<K, V>>,
}

impl<K, V> ThreadSafeLRUCache<K, V>
where
    K: Eq + Hash + Clone,
{
    pub fn new(capacity: usize) -> Self {
        Self {
            cache: Mutex::new(Cache {
                map: HashMap::new(),
                order: VecDeque::with_capacity(capacity),
                capacity,
            }),
        }
    }

    pub fn get(&self, key: &K) -> Option<V>
    where
        V: Clone,
    {
        let mut guard = self.cache.lock().unwrap();
        guard.get(key)
    }

    pub fn put(&self, key: K, value: V) {
        let mut guard = self.cache.lock().unwrap();
        guard.put(key, value);
    }

    pub fn len(&self) -> usize {
        let guard = self.cache.lock().unwrap();
        guard.len()
    }

    pub fn is_empty(&self) -> bool {
        let guard = self.cache.lock().unwrap();
        guard.is_empty()
    }

    pub fn clear(&self) {
        let mut guard = self.cache.lock().unwrap();
        guard.clear();
    }
}

impl<K, V> Cache<K, V>
where
    K: Eq + Hash + Clone,
{
    fn get(&mut self, key: &K) -> Option<V>
    where
        V: Clone,
    {
        if let Some((value, index)) = self.map.get_mut(key) {
            let key_clone = key.clone();
            self.order.remove(*index);
            self.order.push_back(key_clone);
            *index = self.order.len() - 1;
            Some(value.clone())
        } else {
            None
        }
    }

    fn put(&mut self, key: K, value: V) {
        if self.map.contains_key(&key) {
            if let Some((stored_value, index)) = self.map.get_mut(&key) {
                *stored_value = value;
                let key_clone = key.clone();
                self.order.remove(*index);
                self.order.push_back(key_clone);
                *index = self.order.len() - 1;
            }
        } else {
            if self.order.len() >= self.capacity {
                if let Some(oldest_key) = self.order.pop_front() {
                    self.map.remove(&oldest_key);
                }
            }
            self.order.push_back(key.clone());
            self.map.insert(key, (value, self.order.len() - 1));
        }
    }

    fn len(&self) -> usize {
        self.map.len()
    }

    fn is_empty(&self) -> bool {
        self.map.is_empty()
    }

    fn clear(&mut self) {
        self.map.clear();
        self.order.clear();
    }
}

impl<K, V> Clone for ThreadSafeLRUCache<K, V>
where
    K: Eq + Hash + Clone,
    V: Clone,
{
    fn clone(&self) -> Self {
        let guard = self.cache.lock().unwrap();
        let cache_data = Cache {
            map: guard.map.clone(),
            order: guard.order.clone(),
            capacity: guard.capacity,
        };
        Self {
            cache: Mutex::new(cache_data),
        }
    }
}

fn main() {
    println!("Starting Thread-Safe LRU Cache demonstration");

    let cache = ThreadSafeLRUCache::new(3);

    println!("Created LRU cache with capacity 3");

    cache.put("user:1", "Alexey");
    cache.put("user:2", "Maria");
    cache.put("user:3", "Ivan");

    println!("Added 3 users:");
    println!("  user:1 -> {}", cache.get(&"user:1").unwrap_or("Not found"));
    println!("  user:2 -> {}", cache.get(&"user:2").unwrap_or("Not found"));
    println!("  user:3 -> {}", cache.get(&"user:3").unwrap_or("Not found"));

    println!("Cache size: {}", cache.len());
    println!("Is empty? {}", cache.is_empty());

    println!("Accessing user:1 to make it recently used...");
    let _ = cache.get(&"user:1");

    println!("Adding new user (should evict user:2)...");
    cache.put("user:4", "Olga");

    println!("Cache state after adding 4th element:");
    println!("  user:1 -> {}", cache.get(&"user:1").unwrap_or("Not found"));
    println!("  user:2 -> {}", cache.get(&"user:2").unwrap_or("Not found"));
    println!("  user:3 -> {}", cache.get(&"user:3").unwrap_or("Not found"));
    println!("  user:4 -> {}", cache.get(&"user:4").unwrap_or("Not found"));

    println!("Multithreaded access test:");
    
    let cache_clone = cache.clone();
    let thread1 = thread::spawn(move || {
        println!("  Thread 1: Adding data...");
        cache_clone.put("product:a", "Laptop");
        cache_clone.put("product:b", "Smartphone");
        thread::sleep(Duration::from_millis(50));
        println!("  Thread 1: Reading product:a -> {:?}", 
                 cache_clone.get(&"product:a"));
    });

    let cache_clone2 = cache.clone();
    let thread2 = thread::spawn(move || {
        thread::sleep(Duration::from_millis(25));
        println!("  Thread 2: Reading user:1 -> {:?}", 
                 cache_clone2.get(&"user:1"));
        println!("  Thread 2: Adding product:c...");
        cache_clone2.put("product:c", "Tablet");
    });

    thread1.join().unwrap();
    thread2.join().unwrap();

    println!("Final cache size: {}", cache.len());
    
    println!("Cache contents:");
    let test_keys = ["user:1", "user:3", "user:4", "product:a", "product:b", "product:c"];
    for key in test_keys {
        if let Some(value) = cache.get(&key) {
            println!("  {} -> {}", key, value);
        }
    }

    println!("Clearing cache...");
    cache.clear();

    println!("Size after clear: {}", cache.len());
    println!("Is empty? {}", cache.is_empty());

    println!("Demonstration completed");
}
