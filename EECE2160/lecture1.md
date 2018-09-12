## Lecture 1: More C++

#### Randomness

```C++
using namespace std;

int main() {
	int blah;
    
    srand (time(NULL)); // set the random seed
	
    blah = rand() % 50; // randomize number from 0 to 49    	
}
```

#### Arrays

```c++

using namespace std;

int main() {
    int age[5];
    
    srand (time(NULL));  // set the random seed
    
    age[0] = 1;
    age[1] = rand() % 20 // randmize number from 0 to 19
}
```

#### For Loops

```
for (init; condition; increment) {
	conditional code;
}
```

```c++
using namespace std;

int main() {
    
    int forloop1;
    
    for (forloop1 = 1; forloop1 < 10; forloop1 += 1) {
        cout << "Value of forloop1" << forloop1 << endl;
    }    
}
```

#### Stars Formation Exercise

```c++
#include <iostream>
#include <string>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
using namespace std;

int main() {
    int forloop1, forloop2;
  
	for (forloop1 = 1; forloop1 <= 5; forloop1 = forloop1 + 1) {
        for (forloop2 = 1; forloop2 <= forloop1; forloop2++) 
            cout << "*";
	    cout << endl;
    
  	for (forloop1 = 1; forloop1 <= 5; forloop1 = forloop1 + 1) {
        for (forloop2 = 1; forloop2 <= 5 - forloop1; forloop2++)
            cout << "*";
   		cout << endl;      
  	}  
  
	cout << endl << endl;
  	cout << "The Memory Address for forloop1 is: " << &forloop1;
  	cout << "The Memory Address for forloop2 is: " << &forloop2;
  	return 0;
}
```

#### Arrays and For Loops

```c++
using namespace std;

int main() {
    int forloop1;
    int age[5];
    
    srand(time(NULL)); // initialize random seed
    
 	// populate the age array with random numbers from 0 to 99
 	for (forloop1 = 0; forloop1 < 5; forloop1 += 1) {
        age[forloop1] = rand() % 100;
        cout << "The value of age[" << forloop1 << "] is:" << age[forloop1] << endl;
 	}
}
```

* C++ arrays do not stop you from going out of bounds (eg. a ```age[6]``` for an array of size 5 will work)

#### Backwards Copy

```c++
using namespace std;

int main() {
    int age[10];
    int age2[10];
    int forloop1;
    
 	srand(time(NULL));
	
	// populate the age array with random numbers from 0 to 99
	for (forloop1 = 0; forloop1 < 10; forloop1 += 1) {
        age[forloop1] = rand() % 100;
	} 
	
	// reverse copy
    for (forloop1 = 9; forloop1 >= 0; forloop1 -= 1) {
        age2[forloop1] = age[9 - forloop1];
    }
    
    // print both arrays
    for (forloop1 = 0; forloop1 < 10; forloop1 += 1) {
        cout << "Elements at index " << forloop1 << " are :" << age[forloop1] << " and " << age2[forloop1] << endl; 
    }
    
    return 0;
}
```

#### While Loops

```
while (condition) {
    conditional code
}
```

#### Do While Loops

```
do {
    statement
} while (condition)
```

#### Switch

```
switch (expression) {
    case constant-expression :
    	statement(s);
    	break;
    case constant-expression :
    	statement(s);
    	break;  
    // optional
    default :
    	statement(s);
}
```

#### Menu with Switch

```c++
#include <iostream>
#include <string>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
using namespace std;

int main() {
	int response = 1;
  	string answer;

  	
  	while (response != 4) {
	  	cout << "Welcome to Space Station" << endl;
  		cout << "Please choose one of the following menu: " << endl;
  		cout << "(Type a number followed by Enter to make your selection)" << endl;
  		cout << "1. Going to Cafeteria " << endl;
  		cout << "2. Going to Grocery Store " << endl;
  		cout << "3. Going to Garage " << endl;
  		cout << "4. Quit Program " << endl;
  		cout << "Make your selection: ";
  		cin >> response; 

  		switch (response) {

     		case 1: cout << "I am sorry, the Cafeteria is closed for today" << endl << endl;
            break;

     		case 2: cout << "I am sorry, the Grocery Store is still under construction" << endl << endl;
            break;

     		case 3: cout << "Have you made an appointment yet? Y/N: ";
            	cin >> answer;
            	if (answer == "y" || answer == "Y" || answer == "yes" || answer == "YES")
            		cout << "Okay, bring your motor here" << endl << endl;
             	else cout << "I am sorry, you need to make an appointment first" << endl << endl;
             	break;
     
     		case 4: cout << "Terminating Program now" << endl << endl;
             	break;
     
     		default: cout << "Next time please choose either number 1, 2, 3, or 4" << endl;
             	break;           
  		}
  	}
  
  	cout << "Thank you for coming" << endl << endl;
  	cout << "The address for response is: " << &response << endl;
  	cout << "The address for answer is:   " << &answer << endl;  
  	return 0;
}

```

