use core::{cell::UnsafeCell, ptr};

use crate::prelude::*;

pub struct Arena {
    managed: UnsafeCell<Vec<Droppable>>,
}

impl Arena {
    pub const fn new() -> Arena {
        Arena {
            managed: UnsafeCell::new(Vec::new()),
        }
    }

    #[allow(clippy::mut_from_ref)]
    pub fn allocate<'arena, T: 'arena>(&'arena self, value: T) -> &'arena mut T {
        fn drop<T>(ptr: *mut ()) {
            let ptr: *mut T = ptr.cast();
            // SAFETY: This must be the last pointer to `ptr`.
            unsafe {
                ptr::drop_in_place(ptr);
            }
        }

        let ptr = Box::leak(Box::new(value));

        // SAFETY: This is the only active reference to `self`.
        let managed = unsafe { &mut *self.managed.get() };
        managed.push(Droppable::new(
            (ptr as *mut T).cast(),
            Box::new(|x| drop::<T>(x)),
        ));

        ptr
    }

    pub fn collect(&mut self) {
        for value in self.managed.get_mut() {
            (value.drop)(value.ptr);
        }
        self.managed.get_mut().clear();
    }
}

impl Drop for Arena {
    fn drop(&mut self) {
        self.collect();
    }
}

struct Droppable {
    ptr: *mut (),
    drop: Box<dyn FnMut(*mut ())>,
}

impl Droppable {
    const fn new(ptr: *mut (), drop: Box<dyn FnMut(*mut ())>) -> Droppable {
        Droppable { ptr, drop }
    }
}
