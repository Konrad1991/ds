#include <iostream>
#include <memory>
#include <cassert>
#include <cmath>

template<typename T = double>
class Vec {

private:
	size_t size;
	T* ptr;

public:
	Vec() = delete;

	Vec(T val, size_t size_) : size(size_), ptr(nullptr) {
		ptr = new T[size];
		for(size_t i = 0; i < size; i++) ptr[i] = val;
	}

	template<typename R>
	Vec(const Vec<R>& other) : size(other.gs()), ptr(nullptr) {
		size = other.gs();
		ptr = new T[size];
		for(size_t i = 0; i < size; i++) ptr[i] = other(i);
	}

	Vec(const Vec<T>& other) : size(other.gs()), ptr(nullptr) {
		size = other.gs();
		ptr = new T[size];
		for(size_t i = 0; i < size; i++) ptr[i] = other(i);
	}

	~Vec() {
		if(ptr != nullptr) {
			delete[] ptr;
			ptr = nullptr;
		}
	}

	T operator()(size_t idx) const {
		return ptr[idx];
	}

	T& operator()(size_t idx) {
		return ptr[idx];
	}

	size_t gs() const {
		return size;
	}

	template<typename R>
	Vec<T>& operator=(const Vec<R>& other) {
		static_assert(std::is_same<T, R>::value);
		size = other.gs();
		if(ptr != nullptr) delete[] ptr;
		ptr = new T[];
		for(size_t i = 0; i < size; i++) {
			ptr[i] = other(i);
		}
		return *this;
	}

	template<typename R>
	Vec<T>& operator=(const Vec<R> other) {
		static_assert(std::is_same<T, R>::value);
		size = other.gs();
		if(ptr != nullptr) delete[] ptr;
		ptr = new T[];
		for(size_t i = 0; i < size; i++) {
			ptr[i] = other(i);
		}
		return *this;
	}

	template<typename R>
	friend std::ostream& operator<<(std::ostream& os, const Vec<R>& v) {
		for(size_t i = 0; i < v.gs(); i++) {
			os << v(i) << std::endl;
		}
		return os;
	}

	T accum_sum() {
		if(ptr == nullptr) return 0;
		T res = 0;
		for(size_t i = 0; i < size; i++) res += ptr[i];
		return res;
	}

	Vec<T>& operator*(T elem) {
		for(size_t i = 0; i < size; i++) {
			ptr[i] *= elem;
		}
		return *this;
	}

	template<typename R>
	Vec<T>& operator/(R elem) {
		static_assert(std::is_arithmetic<R>::value);	
		T elem_cast;
		if constexpr(!std::is_same<T, R>::value) {
			elem_cast = static_cast<T>(elem);
		} else {
			elem_cast = elem;
		}
		for(size_t i = 0; i < size; i++) {
			ptr[i] /= elem_cast;
		}
		return *this;
	}
};

template<typename T, typename R>
Vec<R> operator+(const Vec<T>& left, const Vec<R>& right) {
	static_assert(std::is_same<T, R>::value);
	assert(left.gs() == right.gs());
	Vec<R> ret(0, left.gs());
	for(size_t i = 0; i < ret.gs(); i++) {
		ret(i) = left(i) + right(i);
	}
	return ret;
}

template<typename T, typename R>
Vec<R> operator-(const Vec<T>& left, const Vec<R>& right) {
	static_assert(std::is_same<T, R>::value);
	assert(left.gs() == right.gs());
	Vec<R> ret(0, left.gs());
	for(size_t i = 0; i < ret.gs(); i++) {
		ret(i) = left(i) - right(i);
	}
	return ret;
}

template<typename T, typename R>
Vec<R> operator*(const Vec<T>& left, const Vec<R>& right) {
	static_assert(std::is_same<T, R>::value);
	assert(left.gs() == right.gs());
	Vec<R> ret(0, left.gs());
	for(size_t i = 0; i < ret.gs(); i++) {
		ret(i) = left(i) * right(i);
	}
	return ret;
}

template<typename T, typename ...Args>
auto sum(T first, Args... args) {
	if constexpr(sizeof...(Args) == 0) {
		return first;
	} else {
		return first + sum(args...);
	}
}

template<typename T, typename ...Args>
auto accum_sum(T& first, Args&... args) {
	if constexpr(sizeof...(Args) == 0) {
		return first.accum_sum(); 
	} else {
		return first.accum_sum() + accum_sum(args...);
	}
}

template<typename T, typename ...Args>
auto vector_mean(T& first, Args&... args) {
	size_t l = sizeof...(Args) + 1;
	auto res = sum(first, args...);
	return res/l;
}

template<typename T, typename R>
auto dot(Vec<T>& one, Vec<R>& two) {
	auto res = one*two;
	return res.accum_sum();
}

template<typename T>
auto ssq(Vec<T>& v) {
	return dot(v, v);
}

template<typename T>
auto magnitude(Vec<T>& v) {
	return sqrt(ssq(v));
}

template<typename T>
Vec<T> vec(T elem, size_t size) {
	return Vec<T>(elem, size);
}

template<typename T>
auto vec(T elem) {
	return Vec(elem);
}

template<typename T, typename R>
auto squared_distance(Vec<T>& l, Vec<R>& r) {
	return ssq(l - r);
}

template<typename T, typename R>
auto distance(Vec<T>& l, Vec<R>& r) {
	return magnitude(ssq(l - r));
}


int main() {
	//std::cout << sum(1, 2, 3) << std::endl;

	Vec<double> v1(3, 2);
	Vec<double> v2(4, 2);
	Vec<double> v3 = v1 + v2;

	double res = accum_sum(v1, v2, v3);
	
	//std::cout << accum_sum(v1, v2, v3) << std::endl;

	//std::cout << v1 << " " << v2 << " " << v3 << std::endl;
	//std::cout << vector_mean(v1, v2, v3) << std::endl;

	Vec<double> one(0, 3);
	Vec<double> two(0, 3);
	one(0) = 1.0;
	one(1) = 2.0;
	one(2) = 3.0;
	two(0) = 4.0;
	two(1) = 5.0;
	two(2) = 6.0;

	std::cout << dot(one, two) << std::endl;	

	std::cout << ssq(one) << std::endl;

	auto three = vec(0.0, 2);
	three(0) = 3.0; three(1) = 4.0;
	std::cout << magnitude(three) << std::endl;
}
