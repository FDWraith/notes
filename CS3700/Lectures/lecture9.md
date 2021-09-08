# Lecture 9

### Inter Domain Routing

- implementing provider policies and creating stable paths
- BGP Routers

ASNs (Autonomous System Networks)

- identified by an ASN number, and there are currently ~65539 advertised ASNs
- All ASs must use the same protocol

Protocol Requirements:

- Scalability
- Flexibility -- cost and routing around failures

BGP (Border Gateway Protocol)

- policy-based routing protocol
- Bellman-Ford path vector
- simple protocol, but complex-manual configuration
- errors screw up traffic globally
- policy driven by cost, not performance

BGP Routing

- Each route has a cost: peer-to-peer is not allowed unless paid for, and customer-to-peer is priority
- Peering Wars:
  - Peer: reduce costs, improved performance, may be the only way to connect
  - Don't Peer: competition, would rather connect to customers, agreements require periodic renegotiation

Two types of BGP

- eBGP (external BGP) between networks
- iBGP (internal BGP) to prevent routing loops within the AS, and to enforce BGP policy, even if OSPF has better route









