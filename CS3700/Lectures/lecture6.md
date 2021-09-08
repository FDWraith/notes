# Lecture 6

### Network Layer

Challenges:

- how to represent addresses
- how to route packets (scalability)
- every network is different

Decision to send packet between networks are made independently hop by hop

Internet Service Model

- best-effort (things may break)
- store-and-forward datagram network

Addressing Schemes (Hierarchy)

- break address into segments, and each part is a different level of specificity
- Ex: Telephone numbers
  - 1-617-373-2117
  - 1 refers to US
  - 617 refers to Eastern Mass
  - 373 refers to Northeastern University
  - 2117 refers to a specific office
- Updates are local (changing 2117)

IP Addressing 

- IPv4: 32-bit addressing
- Usually in dotted notation, each number is a byte
- separates into hierarchical address scheme for routing

IP Address Classes

- Class A: 0 [1-8 network bits] [8- 31 host bits]
  - 127 networks, 16777214 hosts
- Class B: 10 [2-16 network bits] [16-31 host bits]
  - 16398 networks, 65,534 hosts
- Class C: 110 [3- 24 network bits] [24 - 31 host bits]
  - 2097512 networks, 254 hosts

Problem:

- Need to break up A and B classes

Solution:

- Add another layer to the hierarchy
- Internally, manage multiple subnetworks (using subnet mask) and have 1 entry on routing table.

Network mask must be a contiguous set of 1's

Classless Inter Domain Routing (CIDR)

- get rid of IP classes
- use bitmasks for all levels of routing
- Aggregation to minimize FIB (forwarding information base)
- Arbitrary split between network and host
  - bitmask or prefix length
  - example: Northeastern -- 129.10.0.0 with netmask 255.255.0.0
- Aggregation allows us to combine routes together to shrink routing table

IPv4 Datagrams

- totally self-contained
- include all necessary addressing information

Header Fields

- Word 1
  - Version: 4 for IPv4
  - Header length: number of 32-bit words (usually 5)
  - Type of Service: DSCP / ECN (unused)
  - Datagram length: length of header + data in bytes
    - limits packets to 65k bytes
- Word 2
  - Identifier: unique number for the original datagram
  - flags: M flag ie is this the last fragment (0 when yes, 1 when not)
  - offset: byte position of the first byte in the fragment
    - divided by 8
- Word 3
  - time to live (decremetnd by each router)
    - prevent loops
  - protocol: ID of encapsulated protocol 
    - 6 = TCP, 17 = UDP
  - checksum
- Word 4 and 5
  - Source and destination IP addresses

Each network has its own maximum packet size (MTU)

- split datagrams into pieces when MTU is too small
- reassembly on the other size
- See Word 2











