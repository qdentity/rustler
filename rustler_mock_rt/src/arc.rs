use std::ops::Deref;
use std::marker::PhantomData;
use std::ptr::NonNull;
use std::sync::atomic;

const MAX_REFCOUNT: usize = (std::isize::MAX) as usize;

pub struct Arc<T: ?Sized> {
    ptr: NonNull<ArcInner<T>>,
    phantom: PhantomData<ArcInner<T>>,
}

impl<T> Arc<T> {

    pub fn new(data: T) -> Self {
        let x = Box::new(ArcInner {
            strong: atomic::AtomicUsize::new(1),
            data,
        });
        let raw = Box::into_raw(x);
        Arc {
            ptr: unsafe { NonNull::new_unchecked(raw) },
            phantom: PhantomData,
        }
    }

}

impl<T: ?Sized> Arc<T> {

    fn inner(&self) -> &ArcInner<T> {
        unsafe { self.ptr.as_ref() }
    }

    pub fn same(&self, other: &Arc<T>) -> bool {
        self.ptr == other.ptr
    }

    pub unsafe fn increase_rc(&self) {
        let old_size = self.inner().strong.fetch_add(1, atomic::Ordering::Relaxed);
        if old_size > MAX_REFCOUNT {
            std::process::abort();
        }
    }

    pub unsafe fn decrease_rc(&self) {
        if self.inner().strong.fetch_sub(1, atomic::Ordering::Release) == 1 {
            panic!("last reference must be decreased through drop");
        }
        atomic::fence(atomic::Ordering::Acquire);
    }

}

impl<T: Sized> Arc<T> {

    pub fn pack(&self) -> usize {
        self.ptr.as_ptr() as *mut () as usize
    }

    pub fn unpack(val: usize) -> Self {
        let raw = val as *mut () as *mut ArcInner<T>;
        let ptr = NonNull::new(raw).unwrap();
        let arc = Self {
            ptr,
            phantom: PhantomData,
        };
        std::mem::forget(arc.clone());
        arc
    }

}

impl<T: ?Sized> Clone for Arc<T> {
    #[inline]
    fn clone(&self) -> Arc<T> {
        let old_size = self.inner().strong.fetch_add(1, atomic::Ordering::Relaxed);

        if old_size > MAX_REFCOUNT {
            std::process::abort();
        }

        Arc {
            ptr: self.ptr,
            phantom: PhantomData,
        }
    }
}

impl<T: std::fmt::Debug + ?Sized> std::fmt::Debug for Arc<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", &**self)
    }
}

impl<T: ?Sized> Deref for Arc<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &T {
        &self.inner().data
    }
}

impl<T: ?Sized> Drop for Arc<T> {
    fn drop(&mut self) {
        if self.inner().strong.fetch_sub(1, atomic::Ordering::Release) != 1 {
            return;
        }
        atomic::fence(atomic::Ordering::Acquire);
        unsafe {
            Box::from_raw(self.ptr.as_ptr());
        }
    }
}

struct ArcInner<T: ?Sized> {
    strong: atomic::AtomicUsize,
    data: T,
}
