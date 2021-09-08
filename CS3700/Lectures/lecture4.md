# Lecture 4

### Contention MAC Goals

- Share the medium
  - Want one user sending at any given time
  - 2+ results in collisions
  - 0 results in idle channel
- High utilization
  - TDMA is low utilization
- Simple, distributed algorithm

ALOHA

- Stations transmit data immediately
- Receivers ACK all packets
- NO ACK = collision, wait a random time, then retransmit
- Optimized for common case: few senders
- Problem: Low utilization -- 18% throughput

Slotted ALOHA

- start transmissions at fixed slot times
- Hosts may only transmit at beginning of a slot
- Frames either collide completely, or not at all
- 37% throughput
- Problem: Hosts must have synchronized clocks

Broadcast Ethernet

- originally, ethernet was a broadcast tech
- 10Base2 -- over coaxial
- Eventually folded into Hub -- still broadcast medium
- 10BaseT and 100BaseT (T stands for Twisted Pair)

Carrier Sense Multiple Access (CSMA)

- Start transmission only if channel is idle

CSMA / CD (collision detection)

- Stop when collided

Ethernet CSMA/CD

- Wired protocol allows us to sense the medium

  ```
  Sense the carrier
  If (carrier):
  	wait for it to end
  	
  Send a frame and sense for collision
  If (no collision):
  	frame hass been delivered
  	
  If (collision):
  	stop sending
  	perform exponential backoff
  	retransmit
  ```

802.3 Ethernet

- Preamble
  - 7 bytes of 10101010
- SF
  - Start Frame is 10101011 (1 byte)
- Source and Destination 
  - MAC addresses
  - 6 bytes
- Length
  - 2 bytes, to read length of data
- Data 
  - 0-1500 bytes (etc)
- Pad
  - Minimum length of 64 bytes
- Checksum

CSMA / CD Collisiions

- Note: distance, propagation delay, frame length

- Too much distance

  - Senders might think that packet was really sent / not detect collision if they finish sending 

- Packet length

  - If sender sends a really short packet
    - Only the other sender notices the collision
  - Minimum packet size 64 byes
    - To give hosts enough time to detect collision

- Relationship between packet size and cable length

  - $$
    min\_frame\_size \div (2\times bandwidth) \times light\_speed = max\_cable\_length
    $$


Exponential Backoff

- When sender detects collision, send jam signal
  - Make sure all hosts are aware of collision
  - Jam signal is 32 bits long (plus header overhead)
- Exponential backoff operates in multiples of 512 bits (64 bytes)
  - Select k $\in$ [0, $2^n - 1$], where n = number of collisions
  - wait k * 51.2us before retransmission

Maximum Packet Size

- Maximum Transmission Unit (MTU): 1500 bytes
- Pros:
  - bit errors in long packets incur significant recovery penalty
- Cons:
  - More bytes wasted on header information
  - Higher per packet processing overhead
- Datacenters are shifting towards Jumbo Frames
  - 9000 bytes per packet

Today's Ethernet is switched

- CSMA / CD is no longer necessary
- Switches 1996

1 Gbit and 10Gbit Ethernet now common

- 100Gbit on the way
- Uses the same old packet header
- full duplex (send and receive at the same time)
- Auto negotiating (backwards compatibility)
- Can also carry power

## Ethernet vs Wireless

- Ethernet has one shared collision domain
  - All hosts on a LAN can observe all transmissions
- Wireless radios have small range compared to overall system
  - Collisions are local
  - Collision are at the reciever, not the sender
  - Carrier sense (CSMA) plays a differnt role
- 802.11 uses CSMA/CA not CSMA/CD
  - Collision avoidance, rather than collision detection

Hidden Terminal Problem

- radios on same network can't always hear each other
- too far apart to hear each other
- sender-side collision detection is useless

Exposed Terminal Problem

- Carrier sensing is problematic
- No collision on receiving end (because receivers are far enough apart), but sender might sense collision

Reachability in Wireless

- Just because A can reach B, and B can reach C, doesn't mean A can reach D

MACA (Multiple Access with Collision Avoidance)

- 1990
- RTS (reservation packet) to receiver
- Receivers send CTS (clear to use channel)
- Data is sent
- Receiver sends ACK, letting other hosts know that channel is idle
- What if sender does not receiver CTS or ACK
  - Assume collision
  - Enter exponential backoff mode

802.11

- Uses CSMA/CA, not MACA

802.11b

- Uses unlicensed 2.4 Ghz band
- 5.5 and 11 Mbps data rates
  - Practical throughput with TCP is only 5.9 Mbps
- 11 channels (in the US). Only 1, 6, and 11 are orthogonal

802.11a

- 5Ghz band
- 6,9,12,18,24,36,48, 54 Mbps

802.11g

- Uses OFDM (with improved performance (54Mbps)
- backwards compatible with 802.11b
  - b devices cause g networks to fall back to b

802.11n

- MIMO
  - Mutliple send and receive antennas per device
  - data stream is mutliplexed
- maximum 600 Mbps (4x4 config)
- 300 Mbps is more common

802.11ac

- 8x8 MiMO in 5Ghz band, 500 Mbps - 1Gbps rates

802.11 Media Access

- Distributed Coordination Function (DCF)
  - Inter Fram Spacing (IFS)
    - DIFS - low priority, normal data packets
    - PIFS - medium priority, used with PCF
    - SIFS - high priority, control packets (RTS, CTS, ACK, etc)
  - Contention interval: random wait time (to make it fair between priorities)

802.11 is complicated



The Case for Bridging

- Only forward packets to intended receipients
- Reduce broadcasting
- Tradeoff:
  - More complex than hubs
  - Need memory buffers, packet processing hardware, routing tables



