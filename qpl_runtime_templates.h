// Implementation of templates required for the QCL runtime for QPL
#include<stdarg.h>

/* This needs to be called with an explicit type for T since the
   compiler won't be able to deduce this automatically in most
   cases. Example: create_array<int>(3,1,2,3) */
template<class T> 
T** create_array(int length, ...) {
    int count = 0;
    va_list p;

    T **arr = new T*[length];
    va_start(p, length);       
    while(count < length) {
	arr[count++] = static_cast<T*>(va_arg(p, T*)); 
    }
    va_end(p);

    return arr;
}

// Allocate a subset of the quantum heap and return a reference.
// Required parameter is the number of qbits associated with the type.
// NOTE: This is a _very_ _very_ crude memory management (if one wants
// to call it like that)

extern pthread_mutex_t memory_lock;
template<class T> 
T* allocateMem(int length) {
    pthread_mutex_lock(&memory_lock);
    T *alloc = new T(qMemPos, local_mem);
    qMemPos += length;
    pthread_mutex_unlock(&memory_lock);

    return alloc;
}
