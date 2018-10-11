## Lecture 7: Inheritance

```c++
class Base {
    private:
    	int bPrivate;
    public:
    	int bPublic;
    protected:
    	int bProtected
}

class Derived : public Base {
public:
    int Circle() {
    	// Derived classes cannot access private data of base class
	    // bPrivate = 10
    	// Derived classes can access protected data, but outside functions cannot
    	pPublic = 10;
    }
}
```

Constructors cannot by overriden. The base class constructor is called first.

For destructors, it is the opposite; the derived class destructor is called before the base class destructor.



