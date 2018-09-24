### Lecture 5: Data Structures

```c++
struct playerclass {
    int str;
    int agi;
    double luck;
}

int main() {
    playerclass warrior;
    warrior.str = 100;
    warrior.agi = 10;
    warrior.luck = 20;
}
```

Structure is like an array: Warrior is a pointer to the address of the warrior.

```c++
// DANGEROUS METHOD
int main() {
    playerclass *warrior; // memory is not allocated
    warrior->str = 100;
    warrior->agi = 10;
    warrior->luck = 20;
}
```

```c++
int main () {
    playerclass *warrior = new playerclass();
}
```

```c++
int main() {
    playerclass *warrior, war;
    warrior = &war;
    warrior->str = 100;
    warrior->agi = 10;
    warrior->luck = 20;
    
    // Accessing
    // warrior->str
    // (*warrior).luck
}
```

If statement creates a table (memory used), but ternary operation doesn't

```C++
// Ternary Operation
c = a > b ? a : b; 
```

#### Classes

```c++
class Rectangle {
	private:
	   	int width;
	   	int length;
	public:
    	void set(int w, int l) {
            width=w;  length=l;
      	}
	   
    	int area() { 
		   return width * length;
        }
};
```

Declaring function defintions as part of a class

```c++
class Monster {
    private:
    	int patt; //physicalattack; 
    	int matt; //magicattack;
    	int pres; //physicalresistance;
    	int mres; //magicresistance
  	public:
    	void setstats (int patt, int matt, int pres, int mres);
    	int sumofstats();
};

void Monster::setstats (int a, int b, int c, int d) {
    patt = a;
  	matt = b;
  	pres = c;
  	mres = d;
}

int Monster::sumofstats () {
  	int temp = patt + matt + pres + mres;
  	return temp;
}
```

```C++
class Monster {
    private:
    	int sums;
    public:
    	Monster();           // Constructor
    	~Monster();          // Destructor
}

Monster::Monster() {
    cout << "A monster object has been created ..." << endl;
}

Monster::~Monster() {
    cout << "A monster object has been destroyed ..." << endl;
}
```

Delete pointers to objects when you are done

```C++
int main() {
    Monster *firepointer, firedragon;
    firepointer = &firedragon;
    
    // ... Some Code ... //
    
    delete firepointer
}
```

