#include <iostream>
#include <memory>
#include <cassert>

template<typename T>
class Vec {

	private:
	T* ptr;
	size_t size;
	size_t capacity;

	public:
	Vec(size_t size_) : size(size_), capacity(size_) {
		ptr = new T[size];
	}

	template<typename R>
	Vec(const Vec<R>& other) {
		static_assert(std::is_same<T, R>::value);
		this -> size = other.gs();
		this -> capacity = other.gc();
		ptr = new T[size];
		for(size_t i = 0; i < size; i++) {
			ptr[i] = other(i);
		}
	}
	
	Vec() {
		size = 0;
		capacity = 0;
		ptr = nullptr;
	}

	~Vec() {
		if(ptr != nullptr) {
			delete[] ptr;
			ptr = nullptr;
		}	
	}

	size_t gs() const {
		return size;
	}

	size_t gc() const {
		return capacity;
	}

	T operator()(size_t index) const {
		return this -> ptr[index];
	}

	T& operator()(size_t index) {
		return this -> ptr[index];
	}

	friend std::ostream& operator<<(std::ostream& os, const Vec<T>& v) {
		for(size_t i = 0; i < v.gs(); i++) os << v(i) << std::endl;
		return os;
	}

	template<typename R>
	Vec<T> operator+(const Vec<R>& other) {
		static_assert(std::is_same<T, R>::value);
		assert(other.gs() == size);
		Vec<T> ret(size);
		for(size_t i = 0; i < size; i++) {
			ret(i) = ptr[i] + other(i);
		}
		return ret;
	}

	template<typename R>
	Vec<T> operator-(const Vec<R>& other) {
		static_assert(std::is_same<T, R>::value);
		assert(other.gs() == size);
		Vec<T> ret(size);
		for(size_t i = 0; i < size; i++) {
			ret(i) = ptr[i] - other(i);
		}
		return ret;
	}

	template<typename R>
	Vec<T> operator*(const R scalar) {
		static_assert(std::is_same<T, R>::value);
		Vec<T> ret(size);
		for(size_t i = 0; i < size; i++) {
			ret(i) = ptr[i]*scalar;
		}
		return ret;
	}

	template<typename R>
	Vec<T>& operator=(const Vec<R>& other) {

		std::cout << "test" << std::endl;
		static_assert(std::is_same<T, R>::value);
		if(size != other.gs()) {
			delete[] ptr;
			size = other.gs();
			capacity = other.gs();
			ptr = new T[size];
		}

		for(size_t i = 0; i < size; i++) {
			ptr[i] = other(i);
		}

		return *this;
	}

};

template<typename T>
Vec<T> seq(const T start, const T end) {
	static_assert(std::is_arithmetic<T>::value);
	Vec<T> ret(end - start);
	for(size_t i = 0; i < ret.gs(); i++) {
		ret(i) = start + static_cast<T>(i);
	}
	return ret;
}

template<typename T>
Vec<T> rep(const T value, size_t length) {
	static_assert(std::is_arithmetic<T>::value);
	Vec<T> ret(length);
	for(size_t i = 0; i < ret.gs(); i++) {
		ret(i) = value;
	}
	return ret;
}

template<typename Tsingle, typename... T>
Vec<Tsingle> vec_sum(T&... args) {
	size_t i = 0;
	Vec<Tsingle> ret;
	
	([&](auto & arg){
		if(i == 0) {
			ret = arg + rep(0.0, arg.gs());
		} else {
			//ret = 1; // ret + arg;	
		}
		std::cout << arg << std::endl;
		i++;
	}(args), ...);
	
	return ret;
}


int main() {
	Vec<double> v1 = seq(1., 5.);

	Vec<double> v2 = rep(3.14, 4);

	//std::cout << v1 << v2 << std::endl;
	//std::cout << v1 + v2 << std::endl;

	//Vec<int> v3 = rep(3, 4);
	//std::cout << v3 * 7 << std::endl;

	auto v4 = vec_sum<double>(v1, v2);
}
