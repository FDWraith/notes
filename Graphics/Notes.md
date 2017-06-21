## 02-01-2017 | Aim: Bits o' Pixels##

*Do Now*: How would you represent color data if you only had the following amount of space for each pixel?

1. 1 bit  ==  black or white
2. 2 bit  ==  black, white, gray 
   - 2nd bit for intensity
3. 3 bits == rgb (basic)
4. 4 bits == rgba (basic)
5. 6 bits == rgb
   - each color given its own intensity

Color depth -- number of bits used for a pixel

*Revolution in the Valley* - book on development of Macintosh computer

**Image Files**

- File Types
  - Uncompressed 
  - Compressed
  - Lossless -- Direct representation of original data
  - Lossy -- Loss of data through compression
- File extensions
  - gif ( C, L )
  - png( C, L )
  - jpg( C, Ly )
  - bmp( U, L )
  - tif( U, L)

## 02-02-2017 | Aim:  ##

#### Image File Formats ####

**Image Files**

- **Uncompressed** (U) - each pixel has a piece of data 
- **Compressed** (C) - not every pixel has a piece of data attached
- **Lossless** (L) - direct representation of original source
- **Lossy** (LY) - loss of the direct representation

**Image File Extensions ( Raster Format )**

- JPG - ( C, LY )
- PNG - ( C, L )
  - Uses *Run Length Encoding*, which counts the number of repeating characters and etc. 
    - Ex. GGGRPPPP -- > 3G 1R 4P
  - Works well for computer generated images, because there are a lot of repeating color. 
  - Does NOT work well for cameras / photographs of real-world, because shading
- GIF - ( C, L )
  - Lossy initially ( pixelating of images), but lossless when putting it all together 
- BMP - ( U, L )
  - Generally used for maps in games ( don't need to worry about compression )
- TIF - ( C, L )

**Image File Extensions ( Vector Format )**

- SVG
- PDF

**Raster Format** -- Where images are stored as a grid of pixels

**Vector Format** -- Where images are stored as a set of drawing instructions 

#### FORMAT FOR CLASSWORK : NetPBM

- Uncompressed and Lossless 
- *.ppm* file extension
- All whitespace is the same

**Format**

- Header
  - P3 -- Denotes what netPBM file it is 
  - XRES  -- Width of resolution
  - YRES  -- Height of resolution
  - MAX COLOR -- 
- Pixel Data
  - RGB RGB RGB ... ( ASCII Numeric Values )
- NOTE : Newline at the end of the file

## 02-03-2017 | Aim: ##

```$ convert``` can convert an image from one format to another.

## 02-06-2017 | Aim: Bresenham's Line Algorithm  

#### Drawing Lines

- Have to color entire pixels ( in contrast to part of a pixel ) 

- The screen is discrete ( not continuous ) -- Have to deal with integers 

- We care about the pixels themselves, not the grid lines 

  - Center of Pixel is coord of Pixel? 

- Assumption for inital line development:

  - 0 < *m* <= 1 

    - reduces the number of pixels we have to look at to decide if we have to go there

  - Endpoints are known 

  - We are not going backwards

  - Standard Equation for Line: A*x* + B*y* + C = 0

    - f(x,y) = Ax + By + C

      - $$
        A = Δy,
        B = -(Δx),
        C = (Δx)b
        $$

    - f(x,y) = {  0 : if (x,y) is on the line 

      ​		< 0 : if (x,y) is above the line

      ​		> 0 : if (x,y) is below the line 

      ​		} 


## 02-07-2017 | Aim: BresenHam's Line algo. ( Cont. ) 

### Drawing Lines (Cont.)

- Possible Points: (x +1, y), (x+1, y+1)

  - Midpoint: (x+1, y + 0.5) 
    - Check whether midpoint is above or below the line
      - If above, choose the point below
      - If below, choose the point above
      - If 0, choose either, just be consistent  

- ```
  ( x0, y0 ) --> ( x1, y0 )

  x = x0, y = y0
  d = f( x + 1, y + 0.5 )
  while x <= x1:
  	plot( x, y )
      if ( d > 0 ):
      	y++;
      x++;
      d = f(x + 1, y + 0.5 )
  ```

## 02-08-2017 | Aim: BresenHam's Line algo. ( Cont. )

### Drawing Lines ( Cont. )

- Increase efficiency by removing the function inside the while loop:

  - ```
    d = f( x + 1, y + 0.5 )
    while x <= x1:
    	plot( x, y )
    	if( d > 0 ):
    		y++
    		d+=B
    	x++
    	d+=A
    ```

- We can increase efficiency more:

  - ```
    d = f( x0 + 1, y0 + 0.5 )
    d = Ax0 + By0 + C + A + 0.5B
      Becomes 
    d = A + 0.5B ( Don't even need the function anymore )
    ```

- Double Everything ( So no division ):

  - ```
    	plot( x, y )
    	if( d > 0 ):
    		y++
    		d+=2B
    	x++
    	d+=2A
    ```


## 02-16-2017 | Aim:

### Using Edgelists

- Store image information as a list of edges 

- Each pair of entries in the list will define one line

  - [ p0, p1, p2, p3, p4, p5, ... ]
    - {p0, p1}, {p2, p3}, {p4, p5}

- The edge list can be used to apply transformations

- Storing / Representing the edge list

  - Each point is made of x, y, z components

  - Each component is a floating point number  

  - ```
    x0   x1   ...  xN
    y0   y1   ...  yN
    z0   z1   ...  zN
    1.0  1.0  ...  1.0
    ------------------
    4 x N
    ```

- Matrix Math

  - Scalar Multiplication
  - Matrix Multiplication
    - For M * N
      - \# of Cols in M = \# of Rows in N
  - Identity Matrix 

## 02-17-2017 | Aim: MATRICES

### Matrix Multiplication

- ( 4 x N ) * ( N x N ) = ( 4 x N ) *vs* ( 4 x 4 ) * ( 4 x N ) = ( 4 x N )

- $$
  \begin{vmatrix}
  A & B & C \\
  D & E & F \\
  \end{vmatrix}
  * 
  \begin{vmatrix}
  g & h \\
  i & j \\
  k & l \\
  \end{vmatrix}
  =
  \begin{vmatrix}
  (Ag + Bi + Ck) & (Ah + Bj + Cl) \\
  (Dg + Ei + Fk ) & (Dh + Ej + Fl) \\
  \end{vmatrix}
  $$

- #### Multiplicative Identity Matrix

  - $$
    \begin{vmatrix}
    1 & 0 & 0 & 0 \\
    0 & 1 & 0 & 0 \\
    0 & 0 & 1 & 0 \\
    0 & 0 & 0 & 1 \\
    \end{vmatrix}
    *
    \begin{vmatrix}
    x \\
    y \\
    z \\
    1 \\
    \end{vmatrix}
    =
    \begin{vmatrix}
    x \\
    y \\
    z \\
    1 \\
    \end{vmatrix}
    $$

  - I * M = M

  - Square Matrix

  - Mostly filled with 0's 

  - 1's along the diagonal


## 03-01-2017 | Aim: Transformations

### Transformations

- Scaling, Translations, and Rotations

#### Translation

- ```( x, y, z ) --T(a,b,c)--> ( x+a, y+b, z+c )```

- $$
  \begin{vmatrix}
  1 & 0 & 0 & a \\
  0 & 1 & 0 & b \\
  0 & 0 & 1 & c \\
  0 & 0 & 0 & 1 \\
  \end{vmatrix}

  * 

  \begin{vmatrix}
  x \\
  y \\
  z \\
  1 \\
  \end{vmatrix}

  = 

  \begin{vmatrix}
  x + a \\
  y + b \\
  z + c \\
  1 \\
  \end{vmatrix}

  \\

  T * E
  $$
















#### Scaling

- ```( x, y, z ) --S(a,b,c)--> ( x*a, y*b, z*c )``` 

- $$
  \begin{vmatrix}
  a & 0 & 0 & 0 \\
  0 & b & 0 & 0 \\
  0 & 0 & c & 0 \\
  0 & 0 & 0 & 1 \\
  \end{vmatrix}

  * 

  \begin{vmatrix}
  x \\
  y \\
  z \\
  1 \\
  \end{vmatrix}

  = 

  \begin{vmatrix}
  xa \\
  yb \\
  zc \\
  1 \\
  \end{vmatrix}

  \\

  S * E
  $$

- To do an in-place scaling, translate to origin, scale, and then translate back

#### Rotation

- Rotations across different Axes

  - **Z-Axis:**```( x, y, z ) --R(θ,z-axis)--> ( xcosθ - ysinθ, ycosθ + xsinθ, z )```

  - $$
    \begin{vmatrix}
    cosθ & -sinθ & 0 & 0 \\
    sinθ & cosθ & 0 & 0 \\
    0 & 0 & 1 & 0 \\
    0 & 0 & 0 & 1 \\
    \end{vmatrix}

    * 

    \begin{vmatrix}
    x \\
    y \\
    z \\
    1 \\
    \end{vmatrix}

    = 

    \begin{vmatrix}
    xcosθ - ysinθ \\
    ycosθ + xsinθ \\
    z \\
    1 \\
    \end{vmatrix}

    \\

    R * E
    $$

  - **X-Axis:**```( x, y, z ) --R(θ, x-axis)--> ( x, ycosθ - zsinθ, zcosθ + ysinθ )```

  - $$
    \begin{vmatrix}
    1 & 0 & 0 & 0 \\
    0 & cosθ & -sinθ & 0 \\
    0 & sinθ & cosθ & 0 \\
    0 & 0 & 0 & 1 \\
    \end{vmatrix}

    * 

    \begin{vmatrix}
    x \\
    y \\
    z \\
    1 \\
    \end{vmatrix}

    = 

    \begin{vmatrix}
    x \\
    ycosθ - zsinθ \\
    zcosθ + ysinθ \\
    1 \\
    \end{vmatrix}

    \\

    R * E
    $$

  - **Y-Axis:**```( x, y, z ) --R(θ, y-axis)--> ( xcosθ + zsinθ, y, zcosθ - xsinθ)```

  - $$
    \begin{vmatrix}
    cosθ & 0 & sinθ & 0 \\
    0 & 1 & 0 & 0 \\
    -sinθ & 0 & cosθ & 0 \\
    0 & 0 & 0 & 1 \\
    \end{vmatrix}

    * 

    \begin{vmatrix}
    x \\
    y \\
    z \\
    1 \\
    \end{vmatrix}

    = 

    \begin{vmatrix}
    xcosθ + zsinθ \\
    y \\
    zcosθ - xsinθ \\
    1 \\
    \end{vmatrix}

    \\

    R * E
    $$

  - ​

### Combining Transformations

E0, R, S, T

- R * E0 = E1
- S * E1 = E2
- T * E2 = E3 
- E3 = ( T * S * R ) * E0 


## 03-09-2017 | Aim: Parametric Equations

### Parametric Equations

- A way to describe a curve with respect to an independent variable ( *t* )

  - $$
    x = f(t) \\
    y = g(t) \\ 
    ---- \\
    Line  \\
    x = x0 + (Δx)t \\
    y = y0 + (Δy)t \\
    ----\\
    Circle\\
    x = rcos(t) + cx\\
    y = rsin(t) + cy\\
    $$















## 03-10-2017 | Aim: Splines 

**Problem**: Given information does not match coefficients

### Hermite Curves

- Given: P0, P1 (endpoints), M0, M1 (slopes at endpoints) 

- Points on the curve: 

- $$
  f(t) = at^3 + bt^2 + ct + d
  $$

- Slopes at each point:

- $$
  f'(t) = 3at^2 + 2bt + c
  $$

- Finding the coefficients

- $$
  f(0) = d = P0 \\
  f(1) = a + b + c + d = P1 \\
  f'(0) = c = M0 \\
  f'(1) = 3a + 2b + c = M1 \\
  $$

- Using Matrices

- $$
  \begin{vmatrix}
  0 & 0 & 0 & 1 \\
  1 & 1 & 1 & 1 \\
  0 & 0 & 1 & 0 \\
  3 & 2 & 1 & 0 \\
  \end{vmatrix}

  * 

  \begin{vmatrix}
  a \\
  b \\
  c \\
  d \\
  \end{vmatrix}

  = 

  \begin{vmatrix}
  P1 \\
  M0 \\
  M1 \\
  \end{vmatrix}

  \\

  H * C = G

  \\

  ----

  \\

  \begin{vmatrix}
  2 & -2 & 1 & 1 \\
  -3 & 3 & -2 & -1 \\
  0 & 0 & 1 & 0 \\
  1 & 0 & 0 & 0 \\
  \end{vmatrix}

  * 
  \begin{vmatrix}
  P0 \\
  P1 \\
  M0 \\
  M1 \\
  \end{vmatrix}

  = 

  \begin{vmatrix}
  a \\
  b \\
  c \\
  d \\
  \end{vmatrix}

  \\

  H^{-1} * G = C
  $$
















## 03-13-2017 | Bezier Curves

### Bezier Curves

#### Linear Curves

- $$
  P(t) = (1 - t)P0 + tP1
  $$
















#### Quadratic Curves

- $$
  Q(t) = (1 -t)Q0 + tQ1 \\
  Q0 = (1 - t)P0 + tP1 \\
  Q1 = (1- t)P1 + tP2 \\
  ----\\
  Q(t) = (1-t)^2P0 + 2t(1-t)P1 + t^2P2
  $$
















#### Cubic Curves

$$
R(t) = (1-t)R0 + tR1 \\
R0 = (1-t)^2P0 +2t(1-t)P1 +t^2P2 \\
R1 = (1-t)^2P1 + 2t(1-t)P2 + t^2P3 \\
---- \\
R(t) = (1-t)^3P0 + 3t(1-t)^2P1 + 3t^2(1-t)P2 + t^3P3 \\
R(t) = (-P0 + 3P1 -3P2 + P3)t^3 + (3P0 -6P1 + 3P2)t^2 + (-3P0 + 3P1)t + P0 \\
$$

#### Bezier Matrix

$$
\begin{vmatrix}
-1 & 3 & -3 & 1 \\
3 & -6 & 3 & 0 \\
-3 & 3 & 0 & 0 \\
1 & 0 & 0 & 0 \\
\end{vmatrix}

* 
\begin{vmatrix}
P0 \\
P1 \\
P2 \\
P3\\
\end{vmatrix}

= 

\begin{vmatrix}
a \\
b \\
c \\
d \\
\end{vmatrix}

\\

B * G = C
$$

## 03-22-2017 | Aim: 3-D Shapes 

#### Rectangular Prism

- **Given**: One vertex, height, width, and depth
- **Defining Points**: Vertices 
  - Combinations of ``x``, ``x+w``, ``y``, ``y-h``, ``z``, and ``z-d``

#### Sphere

- **Given**: Center, radius

- **Defining Points**: Points on the surface 

- **Generate**: Rotate a circle about the x or y-axis

- $$
  \begin {vmatrix}
  1 & 0 & 0 \\
  0 & cosϕ & -sinϕ \\
  0 & sin\phi & cos\phi \\
  \end{vmatrix}

  *

  \begin{vmatrix}
  rcos\theta \\
  rsin\theta \\
  0 \\
  \end{vmatrix}

  =

  \begin {vmatrix}
  r\cos\theta\\
  r\sin\theta\cos\phi\\
  r\sin\theta\sin\phi
  \end {vmatrix}
  \\\text{ x-rotation                 circle              sphere}\\
  \theta: 0 \rightarrow 2\pi\\
  \phi: 0\rightarrow \pi
  \\\text{or}\\
  \theta: 0 \rightarrow \pi\\
  \phi: 0\rightarrow 2\pi
  $$















### Torus

- **Given**: Center, Radius, Outer Radius

- **Defining Points**: Points on the surface

- **Generate**: Rotate a circle about a point

- $$
  \begin {vmatrix}
  \cos{\phi} & 0 &\sin{\phi}\\
  0 & 1 & 0\\
  -\sin{\phi} & 0 & \cos\phi\\
  \end {vmatrix}
  *
  \begin {vmatrix}
  r\cos\theta + R\\
  r\sin\theta\\
  0
  \end {vmatrix}
  =
  \begin {vmatrix}
  \cos\phi(r\cos\theta + R)\\
  r\sin\theta\\
  -\sin\phi(r\cos\theta + R)
  \end {vmatrix}
  \\\text{ (y-rotation + translation)   circle                       torus}\\
  $$















## 03-30-2017 | aim: Meshes

### Wireframe Meshes

- Defining points are connected to make edges
- Without lighting, we don't lose any detail
- No filling in 
- Don't need to repeat edges 
- Works with our existing edge matrix structure
- Cannot generate good solid objects 

### Polygon Meshes

- Defining points are used as vertices of polygons (triangles) 

- Can be used to generate more realistic 3D objects 

- Can be filled in 

- Cannot use an edge matrix

- Need a new polygon-based drawing system

- Requires more resources than wire frame meshes 

- edge matrix -- > polygon matrix

  - $$[ P_0, P_1, P_2, \ \ \ \ \ \ P_3, P_4, P_5 ... ] $$
  - triangle 0          triangle 1

- ```
  add_box
  add_sphere
  add_torus
  ```

  - adding triangles,
  - points must be added counter-clockwise

- Points for a sphere:

  - semi-circle, so 11 points per slice (if step is 0.1)
  - i, i+11, i+1, i+12


## 04-05-2017 | Aim: Hidden Surface Removal

### Backface Calling

- Ignore any backward-facing polygons

- Important information

  - *V*: vector from the surface of any polygon to the viewer
  - *N*: surface normal ( perpendicular to the plane of the polygon )
  - $$\theta$$ : the angle between *V* and *N*

- Procedure:

  1. Calculate *N*
     - Pick 2 vectors (edges) that share an endpoint, but go in opposite directions
     - *A* = $$P_1 - P_0$$ = $$<x_1 - x_0, y_1 - y_0, z_1 - z_0 >$$ , *B* = $$P_2 - P_0$$ = $$<x_2- x_0, y_2 - y_0, z_2 - z_0 >$$
     - *N* = *A* x *B* ( Cross Product )
     - *N* = $$< a_yb_z - a_zb_y, a_zb_x - a_xb_z, a_xb_y - a_yb_x >$$
  2. Find $$\theta$$ between *V* and *N* 
     - *N* $$*$$ *V* = $$ n_xv_x + n_yv_y + n_zv_z $$ 
     - *N* $$*$$ *V* = $|$ *N* $|$ $|$ *V* $|$ $cos\theta$ 
  3. If $$ -90 < \theta < 90$$, then draw
     - if $$ -90 < \theta < 90$$, then $cos\theta > 0$

  - *V* = $< 0, 0, 1>$ 
  - *N* $$*$$ *V* = $n_zv_z$ 


## 04/19/2017 | Aim: Transformations Extended

### Relative Coordinate System

- Maintain a transformation matrix that defines the current coordinate system
- We will apply the coordinate system to each object as we draw them
- We can explicitly control how and if different objects in our images are connected.
- Use a Stack to store our coordinate systems
- Maintain a stack of coordinate systems
  - push: add a copy of the top to the stack
  - pop: remove the topmost element from the top of the stack
- All transformations are immediately applied to the top of the stack
- Object commands
  1. Generate a polygon list
  2. Apply the top coordinate system
  3. Draw to the screen
  4. Clear the polygon list

## 04-27-2017 | Aim: Compilers

### Compilers

- **Source Code** --> **Compiler** --> **Machine Code**
- **Source Code** --> Lexer --> Parser --> Semantic Analyzer --> Optimizer --> Code Generator --> **Machine Code**

1. **Lexer **
   - Performs lexical analysis

   - Knows all valid tokens in the language

   - Identify the tokens in your code

   - Get rid of all comments

   - *Input*: Source Code

   - *Output*: Token List

     - ```
       int
       main
       (
       )
       {
       long
       x
       = 
       5
       +
       6
       ;
       printf
       (
       )
       ... 
       ```

2. **Parser** 

   - Performs syntax analysis

   - Knows the grammar of the language

   - Syntactic markers used to group elements together to create tree 

   - Finds mismatched symbols, missing symbols, etc. 

   - *Input*: Token List

   - *Output*: Syntax Tree

     - ```
       int --> main --> long --> x --> = --> + --> 5
       										--> 6
       			 --> printf --> "%d"
       			 			--> x
       			 --> return --> 8
       ```

3. **Semantic Analyzer**

   - Knows how to map tokens to operations, values, and identifiers

   - Finds improper operations, wrong invalid for functions, etc. 

   - *Input*: Syntax Tree

   - *Output*: Operation List and Symbol Table (Identifier List)

     - Operation List

     - ```
       int
       long
       +: 5, 6 --> p
       =: x, p
       ```

     - Symbol Table

     - ```
       main: int
       x: long
       printf
       ```

4. **Optimizer**

   - We will not be doing an optimizer

5. **Code Generator**

   - *Input*: Operations List and symbol table

   - *Output*: Machine Code Instructions

     - ```
       101011101000101011110101011111010001010...
       ```


## 05-01-2017 | Aim: Compiler 

### What we need before writing a compiler

- Identify valid tokens
- Define language structure (grammar)
- Map the syntax/tokens to operations/symbols 
- Map the operations to machine code

### **MDL** - Motion Description Language

### Compiler Tools (C)

- ``lex`` - create a lexer 
- ``yacc`` - create a parser / semantic analyzer
- ``flex`` -  free lex
- ``bison`` -  free yacc

### Compiler Tools (Python)

- ``lex.py``
- ``yacc.py``


## 05-02-2017 | Aim: MDL 

### MDL Shenanigans 

- \+ : one or more characters
- \* : zero or more characters 
- ? : zero or one 


## 05-09-2017 | Aim: Animations

### Animations

```
frames 50 //How many Frames
scale 2 2 2 bigenator //bigenator is the knob
var bignenator 0 49 1 0 //0th frame, at step 1; 49th frame, at step 0 
```

```
animate -delay n anim/image*.png
```

New MDL Commands:

​	frames, basename, vary 

### Symbol Table

**Hash Table**

- Keys: Symbol Names
- Values: Type and Value ( List in Python )
- *Ex:* bigenator: [ 'knob', 0 ]

### Setup

- Look for and set frames and basename
- If vary is used, but frames is not, then **exit**
- If frames is used, but basename is not, then set a default basename
  - Compiler message regarding default basename

### Vary

- Generate all the knob values for each frame
- If vary frame range is invalid, then **exit**
- Store all knob values in some data structure
  -  0 --> big: 1, spin: 0
     1 --> big: 0.9, spin: 0.2
     2 --> ......

### Drawing

- For each frame:
  - go through the value structure + set symbol table entries
  - normal drawing, apply knob values when necessary 
  - At the end of the frame, save the image 



## 05-22-2017 | Aim: Filling Polygons

### Methods

- Checking if a point is within a triangle, then fill ( recursively check neighbors )
- Splitting into smaller triangles recursively
- Going between the three points towards the center
- Sweep out the lines from a given point to the opposite side
- Draw lines across two edges to fill 
- Draw vertical / horizontal lines

### Scanline Conversion Algorithm

- Fill a polygon by drawing consecutive horizontal or vertical lines

- Find bottom, middle and top

- $$y$$: By -> Ty
  $$Δy$$: 1

- $$x_0$$: Bx -> Tx
  $$Δx_0$$: $$ (T_x - B_x) / (T_y - B_y) $$

- $$x_1$$: Bx -> Mx
  $$Δx_1$$: $$(M_x - B_x) / (M_y - T_y)$$ 

  ​      Mx -> Tx : if y >= My 

  $$Δx_1$$: $$(T_x - M_x) / (T_y - M_y)$$ 

## 05-26-2017 | Aim: Z-Buffering

### Z-Buffering

- Compare z-values for each pixel and only plot larger values
- Store z-values in a z-buffer ( doubles )
- Modify `plot()` to account fir z-buffer
- Modify `draw_line` to take z-values and calculate z for each pixel
- Calculate z-values in `scanline`


## 05-31-2017 | Aim: Lighting & Shading

### Lighting and Shading

- Color is determined by the 
  - reflective properties of objects
  - color of light(s)
- 2 Kinds of Light Sources
  - Ambient
    - Has no source
    - hits all objects evenly
  - Point light sources

### Calculating Colors

$$I$$ : color value for a surface (illumination)

- $$(I_r,I_g,I_b)$$

Lighting Equation

- $$I = I_{ambient} + I_{diffuse} + I_{specular}$$

### Ambient Reflection

- $$A$$: Ambient light (0-255 RGB)
- $$K_a$$: Constant of ambient reflection (0-1)
- $$I_{ambient} = AK_a$$  

### Diffuse Reflection

- $$L$$: Point light source $$<x,y,z>$$, (0-255)
- $$K_d$$: Constant of diffuse reflection (0-1)
- Reflected Light is evenly distributed in all directions
- $$\theta$$ is the angle between $$N$$ (normal) and $$L$$
- Reflection is strongest when $$\theta = 0$$ 
- $$I_{diffuse} = LK_dcos\theta$$
- $$cos\theta$$
  - ($$N^> * L^>$$): if $$N^>$$ and $$L^>$$ are unit vectors
  - Normalizing a vector creates a unit vector
    - $$V^> = \ < \frac{V_x}{||V||}, \frac{V_y}{||V||}, \frac{V_z}{||V||} >$$
    - $$||V|| = \sqrt{V_x^2 + V_y^2 + V_z^2} $$

### Specular Reflection

- $$L$$: Point Light source
- $$K_s$$: Constant of specular reflection
- Reflected in a specific direction
- $$\alpha$$ is the angle between $$R$$ (reflected vector) and $$V$$ (view vector)
- $$I_{specular} = LK_scos\alpha$$ 
- $$cos\alpha$$
  - ($$R^> * V^>$$)
  - Computing $$R$$
    - $$S = P - L$$
    - $$P = Ncos\theta$$ ( $$\theta$$ is the angle between $$N$$ and $$L$$)
    - $$R = P + S = 2P - L = 2N(N^> * L^>) - L$$
- $$I_{specular} = LK_s[2N(N^> * L_>) - L]^q$$ 
  - $$q$$ is how quickly the reflection fades


### Shading Model

- How often do you find $$I$$

##### Flat Shading

- Calculate $$I$$ once per polygon

##### Goroud Shading

- Generate $$I$$ for each pixel
- Create a list of vertex normal values
  - Vertex Normal: Normalized sum of all surface normals that share a vertex
- Calculate $$I$$ for each vertex normal of a polygon
  - generate new $$I$$ values for scanline and draw-line

##### Phong

- Calculate $$I$$ for each pixel
- create vertex normal list
- lookup vertex normals for each polygon vertex
- Generate new normal values in scanline and draw-line

