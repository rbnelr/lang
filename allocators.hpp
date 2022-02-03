#pragma once
#include <vector>
#include <stdlib.h>

#define aligned_memalloc(size, align) _aligned_malloc(size, align)
#define aligned_free(ptr)             _aligned_free(ptr)

#define _DBG_MAGIC_NONALLOC 0xcc
#define _DBG_MAGIC_UNINIT   0xcd
#define _DBG_MAGIC_FREED    0xdd

#ifdef NDEBUG
	#define _DBG_CLEAR(ptr, val, size)
#else
	#define _DBG_CLEAR(ptr, val, size) memset(ptr, val, size);
#endif

template <typename T, size_t N>
struct smallvec {
	T*     data;
	size_t count;
	size_t capacity;
	
	// Can't use T array or the compiler will call ctor & dtor where it is not supposed to
	// (We placement contruct and destruct manually on resize)
	//T storage[N];
	alignas(T) char storage[N * sizeof(T)];
	
	smallvec (): data{(T*)storage}, count{0}, capacity{N} {
		_DBG_CLEAR(storage, _DBG_MAGIC_NONALLOC, N * sizeof(T));
	}

	smallvec (size_t count): smallvec{} {
		grow_to(count);
	}

	smallvec (std::initializer_list<T> list): smallvec{list.size()} {
		for (size_t i=0; i<list.size(); ++i) {
			data[i] = *(list.begin() + i);
		}
	}

	// no implicit copy
	smallvec (smallvec& other) = delete;
	// no implicit copy
	smallvec& operator= (smallvec& other) = delete;
	
	// probably don't want move ctors for "large" objects like this
	smallvec (smallvec&& other) = delete;
	// probably don't want move ctors for "large" objects like this
	smallvec& operator= (smallvec&& other) = delete;

	~smallvec () {
		_free();
	}

	size_t resize (size_t new_count) {
		size_t old_count = count;

		if (new_count == count) {
			// do nothing
		} else if (new_count > count) {
			grow_to(new_count);
		} else {
			shrink_to(new_count);
		}
		return old_count;
	}

	T* grow_by (size_t n) {
		size_t old_count = count;
		grow_to(count + n);
		return data + old_count;
	}
	void shrink_by (size_t n) {
		assert(count >= n);
		shrink_to(count - n);
	}

	void clear () {
		shrink_to(0);
	}

	void grow_to (size_t new_count) {
		if (new_count <= count)
			return;

		if (new_count > capacity)
			_realloc();

		_DBG_CLEAR(data + count, _DBG_MAGIC_UNINIT, (new_count - count) * sizeof(T));
		
		for (size_t i=count; i<new_count; ++i) {
			// default-construct new elements
			new (&data[i]) T ();
		}

		count = new_count;
	}
	void shrink_to (size_t new_count) {
		if (new_count >= count)
			return;
		
		for (size_t i=new_count; i<count; ++i) {
			// destruct
			data[count].~T();
		}

		_DBG_CLEAR(data + new_count, _DBG_MAGIC_FREED, (count - new_count) * sizeof(T));

		count = new_count;
	}

	static size_t _growfac (size_t old_count) {
		return old_count * 2;
	}
	void _realloc () {
		ZoneScoped;
		
		size_t new_capacity = _growfac(capacity);
		T* new_data = (T*)aligned_memalloc(new_capacity * sizeof(T), alignof(T));
		
		_DBG_CLEAR(new_data, _DBG_MAGIC_NONALLOC, new_capacity * sizeof(T));
		_DBG_CLEAR(new_data, _DBG_MAGIC_UNINIT  , count        * sizeof(T));

		for (size_t i=0; i<count; ++i) {
			// placement move construct new elements
			new (&new_data[i]) T ( std::move(data[i]) );
			// destruct (moved-from) old elements
			data[i].~T();
		}

		_free();

		data = new_data;
		capacity = new_capacity;
	}
	void _free () {
		for (size_t i=0; i<count; ++i) {
			// destruct elements
			data[i].~T();
		}

		if (data == (T*)storage) {
			assert(capacity == N);
			_DBG_CLEAR(data, _DBG_MAGIC_FREED, N * sizeof(T));
		}
		else {
			ZoneScoped;
			aligned_free(data);
		}
	}

	// push_back
	T& push () {
		return *grow_by(1);
	}
	void push (T const& val) {
		*grow_by(1) = val;
	}
	
	T pop_get () {
		assert(count > 0);

		T val = std::move(data[count-1]);
		shrink_to(count-1);
		return val;
	}
	void pop () {
		assert(count > 0);

		shrink_to(count-1);
	}

	_FORCEINLINE T& operator[] (size_t idx) {
		assert(idx < count);
		return data[idx];
	}
	_FORCEINLINE T const& operator[] (size_t idx) const {
		assert(idx < count);
		return data[idx];
	}

	_FORCEINLINE T* begin () {
		return data;
	}
	_FORCEINLINE T* end () {
		return data + count;
	}
	_FORCEINLINE T const* begin () const {
		return data;
	}
	_FORCEINLINE T const* end () const {
		return data + count;
	}
};

#define DECLC _FORCEINLINE constexpr
#define DECL  _FORCEINLINE

template <typename T>
struct arrview {
	T*     data;
	size_t count;
	
	DECLC arrview (): data{nullptr}, count{0} {}

	// raw ctor
	DECLC arrview (T* data, size_t count): data{data}, count{count} {}

	// c-style array ctor
	template <size_t N>
	DECLC arrview (T (&arr)[N]): data{arr}, count{N} {}

	// vector ctor
	DECLC arrview (std::vector<T>& vec): data{vec.data()}, count{vec.size()} {}

	// smallvec ctor
	template <size_t N>
	DECLC arrview (smallvec<T,N>& svec): data{svec.data}, count{svec.count} {}
	
	// copy ctor
	DECLC arrview (arrview& r): data{r.data}, count{r.count} {}
	// copy operator
	DECL arrview& operator= (arrview& r) {
		data = r.data;
		count = r.count;
		return *this;
	}
	// move ctor
	DECLC arrview (arrview&& r): data{r.data}, count{r.count} {}
	// move operator
	DECL arrview& operator= (arrview&& r) {
		data = r.data;
		count = r.count;
		return *this;
	}

	DECL ~arrview () {}

	DECL T& operator[] (size_t idx) {
		assert(idx < count);
		return data[idx];
	}
	DECL T const& operator[] (size_t idx) const {
		assert(idx < count);
		return data[idx];
	}

	DECL T* begin () {
		return data;
	}
	DECL T* end () {
		return data + count;
	}
	DECLC T const* begin () const {
		return data;
	}
	DECLC T const* end () const {
		return data + count;
	}
};

#undef DECL
#undef DECLC

template <typename T>
inline arrview<T> slice (arrview<T> arr, size_t first, size_t count) {
	assert(first + count <= arr.count);
	return arrview<T>(arr.arr + first, count);
}


struct BumpAllocator {
	static inline constexpr size_t BLOCK_SZ = 1024 * 1024 * 4;
	static inline constexpr size_t LARGE_ALLOC_SZ = 1024 * 1024 / 4;

	static inline constexpr size_t BLOCK_ALIGN = 64;

	char* cur = nullptr;
	char* end = nullptr;

	// both blocks and large allocs
	struct Allocation {
		char*  ptr;
		size_t size;
	};
	std::vector<Allocation> allocations;

	BumpAllocator () {
		allocations.reserve(32);
	}
	~BumpAllocator () {
		reset();
	}

	size_t used_bytes () {
		size_t total = 0;
		for (auto& alloc : allocations)
			total += alloc.size;

		total -= end - cur; // substract the unused amount of bytes _only_ from the last block
		return total;
	}

	static inline void* align_ptr (void* p, size_t align) {
		return (void*)( ((uintptr_t)p + (align-1)) & ~(align-1) );
	}

	template <typename T>
	T* alloc () {
		T* ptr = (T*)alloc(sizeof(T), alignof(T));
		new (ptr) T ();
		return ptr;
	}

	// WARNING: ctor not called due to performance concerns
	template <typename T>
	T* alloc_array (size_t count) {
		return (T*)alloc(sizeof(T)*count, alignof(T));
	}

	inline char* alloc (size_t size, size_t align) {
		// code would work fine with size=0, but maybe avoid returning a pointer to _no_ data
		// esp. since it actually does align the pointer to align and
		// potentially triggers new block allocation for no reason
		if (size == 0)
			return nullptr;

		// since allocs larger than BLOCK_SZ are impossible
		// and allocs close to the size of a page already waste space at the end of a page
		// have large allocs be normal malloc calls that are tracked seperately
		if (size >= LARGE_ALLOC_SZ)
			return large_alloc(size, align);

		cur = (char*)align_ptr(cur, align);

		char* ptr = cur;
		cur += size;

		if (cur <= end)
			return ptr;

		return alloc_from_new_block(size, align);
	}

	// new block is needed, slow path so use _NOINLINE to help the compiler pick the fast path for inlining
	_NOINLINE char* alloc_from_new_block (size_t size, size_t align) {
		ZoneScoped;

		add_block();

		cur = (char*)align_ptr(cur, align);

		char* ptr = cur;
		cur += size;

		if (cur <= end)
			return ptr;

		assert(false);
		return nullptr;
	}

	char* large_alloc (size_t size, size_t align) {
		char* ptr = (char*)aligned_memalloc(size, align);
		
		assert((char*)align_ptr(ptr, align) == ptr);

		allocations.push_back({ ptr, size });
		return ptr;
	}

	void add_block () {
		cur = (char*)aligned_memalloc(BLOCK_SZ, BLOCK_ALIGN);
		end = cur + BLOCK_SZ;

		allocations.push_back({ cur, BLOCK_SZ });
	}
	void reset () {
	#ifndef NDEBUG
		printf("g_allocator: used bytes: %llu\n", used_bytes());
	#endif


		for (auto& alloc : allocations)
			aligned_free(alloc.ptr);

		allocations.clear();

		cur = nullptr;
		end = nullptr;
	}
};

inline BumpAllocator g_allocator;
