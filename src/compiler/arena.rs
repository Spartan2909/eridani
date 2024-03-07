use core::{cell::UnsafeCell, ptr::NonNull};

use crate::prelude::*;

pub struct Arena<'arena> {
    managed: UnsafeCell<Vec<NonNull<dyn Droppable + 'arena>>>,
}

impl<'arena> Arena<'arena> {
    pub const fn new() -> Arena<'arena> {
        Arena {
            managed: UnsafeCell::new(Vec::new()),
        }
    }

    #[allow(clippy::mut_from_ref)]
    pub fn allocate<T: 'arena>(&self, value: T) -> &'arena mut T {
        let value = Box::leak(Box::new(value));
        let ptr = value as *mut T;

        let value = value as &mut dyn Droppable;

        // SAFETY: This is the only active reference to `self`.
        let managed = unsafe { &mut *self.managed.get() };
        managed.push(NonNull::from(value));

        unsafe { &mut *ptr }
    }

    pub fn collect(&mut self) {
        for &mut value in self.managed.get_mut() {
            unsafe {
                drop(Box::from_raw(value.as_ptr()));
            }
        }
        self.managed.get_mut().clear();
    }
}

impl<'arena> Drop for Arena<'arena> {
    fn drop(&mut self) {
        self.collect();
    }
}

trait Droppable {
    fn drop(self: Box<Self>) {}
}

impl<T> Droppable for T {}
