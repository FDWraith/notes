# Lecture 7

### IF Fragment Reassembly

- packets assembled based on the offset information in each packet
- challenges
  - out-of-order fragments
  - duplicate fragments
  - missing fragments
  - overlapping fragments

### Fragmentation Concepts

- Highlights many key Internet characteristics
- Decentralized and heterogenous
- connectionless datagram protocol
  - each fragment has full routing information
  - fragments can travel independently, on different paths
- best effort network -- packets can be silently dropped
- most work is on the end of the network (receiving end)

Fragmentation is expensive

- want to avoid as much as possible
- MTU discovery protocol 
  - send packet with "don't fragment" bit set
  - decrease the size until one arrives (that gets through the entire network)

### IPv6

- came about since IPv4 has only about 4B addresses, not enough for every single person
- 8 groups of 16-bit values, separated by ':'
- Leading gzeroes in each group may be omitted
- Groups of zeroes can be omitted to '::'

Localhost in IPv6

- ::1

IPv6 Header

- double the size of IPv4
- Several header fields missing
  - header length -- rolled into Next Header field
  - checksum -- no need 
  - identifier, flags, offset
    - IPv6 routers do not support fragmentation
    - Hosts are expected to use path MTU discovery
- Reflects changing Internet priorities
  - Making networks more homogeneous
  - routing cost and complexity dominate
- prevents DoS attacks that use streams of fragments

Performance Improvements

- Simplified routing tables
  - no need for CIDR (really large subnets)
- Unlikely collisions:
  - lots of addresses

Deployment Challenges

- Switching to IPv6 is a whole-Internet upgrade
- All routers + all hosts have to be changed
- New Control protocols:
  - ICMPv6, DHCPv6, DNSv6

Consequences

- Too many IP addresses, so IP blacklists don't work very well for bots / spammers

Progress

- All network edges are IPv6 ready
- Internet core is hard to upgrade

Transition technologies

- Using tunnels to encapsulate
- 6to4 address always start with 2002:
  - Doesn't work if you are behind a NAT

### Wireshark

- Protocol  Analyzer
- Supports
  - data capture from any interface
  - offline processing 
  - filtering language for select protocols or types of packets
  - library of filters for popular network protocols
- Stable: v3.0.5









