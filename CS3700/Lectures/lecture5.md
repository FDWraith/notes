# Lecture 5

Bridges

- memory buffer for contention for a port to queue packets
- All inputs can talk at the same time
- Original form of Ethernet switch
- Goals
  - Reduce collision domain
  - complete transparency: "plug and play", self-configuring, should not change existing LAN operations
- Roles
  - Forwarding of frames
  - Learning of (MAC) addresses
  - Spanning Tree Algorithm (to handle loops)

Learning Addresses

- Manual configuration is possible, but
  - time consuming
  - error prone
  - not adaptable (hosts get added or removed)
- Learn addresses using a simple heuristic
  - look at the source of frames that arrive on each port
  - record the addresses in frame forwarding table
- Delete old record after a timeout
- Won't forward back to the same port
- If a bridge doesn't know where to send frame, sends to every other port

Removing Loops

- 802.1 uses Spanning Tree Algorithm
- Bridges send BPDUs to determine root of the tree, and shortest path to root
- Root has smallest BID (random number for every bridge)

Bridges vs Switches

- Switch is a speical case of a bridge
  - Each port is connected to a single host
  - No collision domains -- no need for CSMA/CD
  - Can have different speeds on each port

Switching

- Network routing based on MAC addresses
- Learn routes to new hosts automatically
- Resolve loops







