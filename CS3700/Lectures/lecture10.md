# Lecture 10

### Transport Layer

- First connection that is only read by source and destination -- not a hop-to-hop based structure
- Assigning ports to each running application

### UDP

- simple, connectionless datagram - no idea of sequences of packets
- 16-bits of port information for source and destination, ~65535 possible ports
- checksum for error detection
- payload length
- 64-bit header
- Built after TCP, but provides the following instead
  - Not all applications can be built on TCP
  - Custom procotocols can be built on UDP

### TCP

- Reliable, in order packet streams
- Need to attach sequence numbers and other details to the header
- Form a connection with a three-way handshake
- TCP Flags
  - SYN - Synchronization, used for connection setup
  - ACK - acknowledge received data
  - FIN - finish, used to tear down connection
- Three-way handshake
  - each side notifies the other with its starting sequence number
  - sends back an ACK of the SEQ number + 1
- Tear down
  - Either side can initiate tear down
  - Other side may continue sending data
- Sliding Window
  - ACK will only send the **last** contiguous packet
  - if 4, 5, 6 are sent, but 5 is lost, it will send an ACK of 4 each time until sender sends 5.
  - Sends up to window-size number of unacknowledged packets
- Retransmission Time Out (RTO)
  - linked to round-trip time (RTT)
  - measure the time needed for round trip by taking a moving average

### Congestion

- Load on network > capacity
- results in packet loss (finite buffers in routers, eventually some get dropped)
- Router queue build up, delay increases

Congestion Window

- controls/limits how many un-acked data can be sent out on the sent out from the sender

Detect Congestion

- packet dropping is most reliable signal for packet dropping (in wired networks)
- how to detect packet drops: ACKs

Rate adjustment of congestion window

- when ack is received, increase cwnd
- when data is loss, decrease cwnd
- concerns: fairness and efficiencies throughput (other networks use the system as well)
- Additive increase, multiplicative decrease -- fair and stable

Congestion Control 

- Slow Start method
  - cwnd = 1, ssthresh = adv_win, and double cwnd for received acks until packet loss, or cwnd >= ssthresh
- Congestion Avoidance
  - slow start ramp up, and then if cwnd >= ssthresh, ramp up 1/cwnd at a time.
- Timeout
  - Restart, and lower the sshthresh by half

TCP Reno

- Fast retransmit
  - When 3 duplicate acks are received, there was a packet loss, but no need to completely restart. cwnd = ssthresh / 2





