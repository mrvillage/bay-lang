use core::str;
use std::sync::atomic::AtomicU64;

use dashmap::DashMap;

use crate::prelude::*;

pub struct Store<T: InitStore> {
    pub items:   DashMap<u64, T>,
    pub next_id: AtomicU64,
}

pub trait InitStore
where Self: Sized
{
    fn init_store(store: &mut Store<Self>);
}

impl<T> Store<T>
where T: InitStore
{
    pub fn new() -> Self {
        let mut store = Self {
            items:   DashMap::new(),
            next_id: AtomicU64::new(0),
        };
        T::init_store(&mut store);
        store
    }

    pub fn all(&self) -> Vec<&'static T> {
        unsafe {
            let mut items = Vec::with_capacity(self.items.len());
            for item in self.items.iter() {
                items.push(&*(&*item as *const T));
            }
            items
        }
    }

    pub fn all_mut(&self) -> Vec<&'static mut T> {
        unsafe {
            let mut items = Vec::with_capacity(self.items.len());
            for mut item in self.items.iter_mut() {
                items.push(&mut *(&mut *item as *mut T));
            }
            items
        }
    }

    pub fn get(&self, id: u64) -> Option<&'static T> {
        unsafe { self.items.get(&id).map(|x| &*(&*x as *const T)) }
    }

    pub fn get_mut(&self, id: u64) -> Option<&'static mut T> {
        unsafe {
            self.items
                .get_mut(&id)
                .map(|mut x| &mut *(&mut *x as *mut T))
        }
    }
}

impl<T> Default for Store<T>
where T: InitStore
{
    fn default() -> Self {
        Self::new()
    }
}

pub static mut FILE_STORE: Option<Store<(std::path::PathBuf, String)>> = None;

impl InitStore for (std::path::PathBuf, String) {
    fn init_store(store: &mut Store<Self>) {
        let default_file = std::path::PathBuf::from("stdin");
        store.items.insert(0, (default_file, String::new()));
        store.next_id.store(1, std::sync::atomic::Ordering::Relaxed);
    }
}

pub fn get_file_content(id: u64) -> &'static str {
    unsafe {
        FILE_STORE
            .as_ref()
            .unwrap()
            .items
            .get(&id)
            .map(|x| str::from_utf8_unchecked(std::slice::from_raw_parts(x.1.as_ptr(), x.1.len())))
            .expect("File not found")
    }
}

pub fn add_file(path: impl Into<std::path::PathBuf>) -> Result<(u64, &'static str)> {
    let path = path.into();
    let content = std::fs::read_to_string(&path)?;
    unsafe {
        let store = FILE_STORE.as_ref().unwrap();
        let id = store
            .next_id
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        store.items.insert(id, (path, content));
        Ok((id, get_file_content(id)))
    }
}

pub fn get_file_path(id: u64) -> &'static std::path::Path {
    unsafe {
        FILE_STORE
            .as_ref()
            .unwrap()
            .items
            .get(&id)
            .map(|x| &*(x.0.as_path() as *const std::path::Path))
            .expect("File not found")
    }
}
