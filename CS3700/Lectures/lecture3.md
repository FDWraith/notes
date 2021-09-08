# Lecture 3

### Layering & Distribution (Cont)

"End to End Arguments in System Design" - Saltzer, Reed and Clark

- Some applications have end-to-end requirements (security, reliability, etc.)
- Difficult: end-to-end is hard
- End hosts 
  - Can't trust / depend on network 
  - must rely on network to deliver
- Full Functionality built at App level
  - End-to-end checks, retry on failure
- Consequences
  - Same host complexity
  - increases network complexity
  - Overhead
- Don't implement anything in the network that can't be implemented correctly by the hosts
  - Step carefully if you do
- network layer == minimal

Layering & E2E principals regularly violated

- Firewalls, Transparent Proxies, NATs

### Physical Layer

Key Challenges:

- Analog to Digital
- High-Speed bitrate

Two Discrete Signals

- high vs low
- encode 1 to 0

Transmission

- synchronous (time-based)
- amplitude, and frequency matter

Non-Return to Zero (NRZ)

- 1 (high signal)
- 0 (low signal)
- Problem
  - Distinguish long strings of 0's or 1's
  - Desynchronization with clock skew

Non-Return to Zero Inverted (NRZI)

- 1 make transition
- 0 remain the same
- Solves the problem for sequences for 1s, but not 0s
  - long zeroes might indicate the system has died

4-bit/5-bit (100 Mbps Ethernet)

- Encode all 4-bit seq as 5-bit with no more than one leading 0 and two trailing 0
  - never have a run with more than 3 zeroes at any one time
- Efficiency drops to 80%
- 8-bit / 10-bit used in Gigabit Ethernet 

Manchester

- 1 high to low
- 0 low to high
- Solves clock skew (every bit is a transition)
- Problem: halves throughput (two clock cycles per bit)

Physical Layer is the lowest

- not worried about where to place functionality

### Data Link Layer

Function

- Send blocks of data (frames) between physical devies
- regulate access to physical media

Key challenge

- Delineate frames
- Detect errors
- Media Access Control (MAC)
- collisions and recovery

Framing

- data boundaries must be known so headers can be read
- Types of framing
  - byte-oriented protocols
  - bit-oriented protocols

Byte Oriented

- Byte Counting:
  - Sender inserts length of data in bytes at beginning of each frame
  - Receiver extract the length and read that many bytes
- Sentinel Approach
  - Add START and END sentinels to the data
  - Problem: if END is in the data
    - Added a special DLE (data link escape) character before END
    - DLE must also be escaped
  - Used by Point-to-Point, DSL, Modem, cellular

Bit Oriented

- Bit Stuffing
  - Add sentinels to the start and end
  - Both are the same (Ex: 01111110) in HDLC
  - Sender: insert a 0 after each 11111 in data
  - Receiver
    - 111110 -> remove the 0
    - 111111 -> look at one more bit
      - 1111110 -> end of frame
      - 1111111 -> error: Discard frame
  - Worse Case: 17% overhead 

Clock-Based Framing: SONET

- Synchronous Optical Network -> Transmission over very fast optical links
- Fixed-size frames
  - Ex: STS-1 frames are 9x90 = 810 bytes
  - Overhead + Payload 
- Physical layer details
  - bits are encoded using NRZ
  - Payload is XORed with a pseudorandom 127-bit patten to avoid long sequences of 0 or 1

Dealing with Noise

- Detect bit-errors in transmissions?
- How to recover from errors?

Naive Error Detection

- Send two copies of each frame
- Extremely high overhead
- Poor protection: twice the data means twice the chance for errors

Parity Bits

- Add extra bits to keep the number of 1s even
- Example: 7-bit ASCII characters + 1 parity bit
  - 0101001 => 01010011
  - 1101001 => 11010010
- Detects 1-bit errors
- Not reliable against bursty errors (where multiple bits are affected)

Two-Dimensional Parity

- Parity bit for each row, Parity bit for each column, parity for parity byte
- Can detect all 1-,2-and 3-bit erorrs
- 14% overhead

Checksums

- Add the bytes in the data
- Include the sum in the frame
- Less overhead than parity
- Not-resilient to errors
- UDP, TCP, IP

Cyclic Redundancy Check (CRC)

- Uses field theory to compute a semi-unique value for a given message
- Much better performance
- - fixed size overhead per frame
  - quick to implement in hardware
  - Only 1 in 2^32^ chance of missing an error with 32-bit CRC
- Cryptographic hashes are more common
  - eg. ~~MD5~~, ~~SHA1~~, SHA256, SHA512

Reliability

- How does a sender know that a frame was received?
- Receiver sends a ACK (acknowledge message) back

Stop and Wait

- Simplest form of reliability
- Example: Bluetooth
- Timeout duration, to check if ACK has been received
- If ACK not recieved, then keep sending last frame
- Problems:
  - Can only send one frame at a time

Sliding Window

- Send multiple un-ACKed frames
- Number of un-ACKed frames is called the window
- Example: TCP

Data Link Error-Checking

- Usually not necessary, E2E
- Most Useful over lossy links
  - Wifi, cellular, satellite

Media Access

- Ethernet and Wifi are multi-access tech
- Shared by many hosts
- Collisions amongst simultaneous transmissions
  - Destructive Interference
- MAC protocol
  - rules on how to share the medium

Strategies for MAC

- Channel partitioning
  - divide into small pieces
  - allocate each piece to one host
  - Examples: Time Division Multi-Access (TDMA) cellular, Frequency Division Multi-Access (FMDA) cellular
- Taking turns
  - Tightly coordinate shared access to avoid collisions
  - Examples: Token ring networks
- Contention
  - Allow collisions, but use strategies to recover
  - Examples: Ethernet, Wifi

Contention MAC

- Simple, distributed algorithm
- Multiple hosts cannot directly coordinate

Contention Protocol Evolution

- ALOHA
- Slotted ALOGA
  - start transmissions only at fixed time slots
- Carrier Sense Multiple Access (CSMA)
  - Start transmission only if the channel is idle
- CSMA Collision Detection