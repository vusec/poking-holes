# Poking holes

This is the source code for the EAP/PAP attacks described in the
USENIX Security'16
[paper](https://www.usenix.org/system/files/conference/usenixsecurity16/sec16_paper_oikonomopoulos.pdf)
Poking Holes in Information Hiding.

## Abstract

ASLR is no longer a strong defense in itself, but it still serves as a
foundation for sophisticated defenses that use randomization for
pseudo-isolation. Crucially, these defenses hide sensitive information
(such as shadow stacks and safe regions) at a random position in a
very large address space. Previous attacks on randomization-based
information hiding rely on complicated side channels and/or probing of
the mapped memory regions. Assuming no weaknesses exist in the
implementation of hidden regions, the attacks typically lead to many
crashes or other visible side-effects. For this reason, many
researchers still consider the pseudo-isolation offered by ASLR
sufficiently strong in practice.

We introduce powerful new primitives to show that this faith in
ASLR-based information hiding is misplaced, and that attackers can
break ASLR and find hidden regions on 32 bit and 64 bit Linux systems
quickly with very few malicious inputs. Rather than building on memory
accesses that probe the allocated memory areas, we determine the sizes
of the unallocated holes in the address space by repeatedly allocating
large chunks of memory. Given the sizes, an attacker can infer the
location of the hidden region with few or no side-effects. We show
that allocation oracles are pervasive and evaluate our primitives on
real-world server applications.

## What's included

The code implements the attacks in two executables:

- dissect: implements the binary search EAP attack (plus the
  associated PAP-based hole plugging)
- monparnes: implements the state-forking PAP-only attack described in
  the appendix of the paper

Also included

- maps: utility program to display gaps and mapped regions based on
the contents of /proc/$pid/maps
- stub: generic allocation stub. It implements a simple protocol and
can be used either interactively, for experimentation, or in batch
mode

## Implementation details

The OCaml code talks to the memory allocation stub (i.e. the "remote"
code which implements the stub protocol) via the Live_stub
module. However, while developing the PAP-only attack, it was useful
to implement a shadow stub too, which only emulates the kernel
behavior (hence is much faster) and allows for replaying
execution (to test bugfixes). Unfortunately, the shadow stub
implementation had to be set aside towards the end of this project.
