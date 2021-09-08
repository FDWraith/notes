# Lecture 8

### Network Layer Control Plane

- establish how to route across infra

Internet Routing

- Two-tiered hierarchy
  - Autonomous Systems (AS) - first level
    - Region under singel administrative domain
    - Uses intra-domain routing internally (RIP and OSPF)
  - Connections between AS use inter-domain routing (BGP)

- routing algorithms are not efficient enough to execute on entire Internet topology

- different organizations may use diff routing policies / want to hide internal structure

Routing on a Graph

- Goal: find a "good" path on network from src to dest
- Good can mean:
  - shortest path
  - load balanced
  - lowest cost
- Network as a Graph (V = routers, E = links)

### Intra-domain Routing

- Link State: OSPF - open shortest path first, based on Dijkstra
  - Per router computation to determine full routes
  - periodic flooding to get reachability information
- Distance Vector: RIP - based on bellman-ford

Link State Routing

- each node knows its connectivity and cost --> shares this with neighbors through flooding
- Use Dijkstra to compute shortest paths
- Flooding Details
  - LSP (Link State Packet)
  - ID of node generating LSP
  - List of direct neighbors and costs
  - Sequence number (64-bit, assumed to never wrap) -- versioning
  - time to live
- Two different implementations:
- OSPF 
  - Favored by companies, datacenters
  - More optional headers
  - Built on IPv4
- IS-IS
  - favored by ISPs
  - less network overhead and supports more devices
  - IP-agnostic, works on IPv4 and IPv6

Distance Vector Routing

- current best known cost
- exchange vectors among neighbors to learn about lowest cost paths
- you're only sending information to your neighbor
- iterate until path information is clear



