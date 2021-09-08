# Lecture 2

### History of Internet (Cont)

The ARPA Network ==> beginnings of Internet

- Connect computers together
- Different computers, but network connected
- 1973: Satellite Connections

Problem: Early Networks used incompatible protocols

Solution:

- Routers connect networks (middle layer)
- No global control
- Development of IP (internet protocol)

Takeaways

- communication is fundamental to human nature
- key concepts have existed for a long time
- speed/bandwidth, latency, switching, encoding, cable management, multiplexing, routing, security, privacy

---

### Internet Architecture (Layer Cake + Hourglass)

Network Functionality

- Built from many components:
  - Networking technologies (Ethernet, Wifi, DSL)
  - Networking styles (wireless wired, etc.)
  - Applications
- Problem Scenario:
  - Applications & Networking technologies need to talk to each other without having to develop communication between each application and networking technology
  - Solution: Create a network abstraction layer, to develop a common method of communication across both applications & networking technologies

Layered Network Stack

- Applications, Layer N, ... , Layer 2, Layer 1, Physical Media
- Modularity:
  - Functionality is the only important aspect; implementation details left to the specific layer
- Encapsulation:
  - Interfaces define cross-layer interaction
- Flexibility:
  - Reuse of code across the network
  - Implementation can change, interfaces stay the same
- Trade-offs:
  - Speed
  - Hidden Information

OSI Model (Open Systems Interconnect Model)

- Layers:
  - Application 
  - Presentation
  - Session
  - Transport
  - Network
  - Data Link
  - Physical 
- Hosts have all 7 layers
- Routes have Network-Data Link-Physical layers
- Layers communicate peer-to-peer

Physical Layer

- Service
  - Move information between two physically connected systems
- Interface
  - Specifies how to send one bit
- Protocol
  - Encoding scheme for one bit
  - Voltage, Signal Timing
- Examples:
  - fiber optics, coaxial cable

Data Link Layer

- Service
  - Boundaries between packets
  - Media Access Control (MAC)
  - Reliability + Flow Control
- Interface
  - Sends packets between two hosts using same media
- Protocol
  - Physical addressing (MAC address)
- Examples:
  - Ethernet, Wifi, DOCSIS

Network Layer

- Service
  - Packet scheduling
  - Deliver packets across the network
  - fragmentation / reassembly of packets
  - buffer management
- Interface
  - Send packet to specific destination
- Protocol
  - Global Unique Adresses
  - Maintain routing tables
- Example:
  - IP, IPv6

Transport Layer

- Service
  - Multiplexing/demultiplexing connections
  - Congestion control
  - Reliable delivery
- Interface
  - Send message to a destination port
- Protocol
  - Port numbers
  - Reliability / error correction
  - flow-control information
- Examples
  - UDP, TCP

Session Layer

- Service
  - Access management
- Protocol
  - Token management
  - Insert checkpoints

Presentation Layer

- Service
  - Convert data between representations
- Protocol
  - define data formats

Application Layer

- Everything else

