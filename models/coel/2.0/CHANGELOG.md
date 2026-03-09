# CHANGELOG

## COEL Model v2.0

### Correction of duplicate wound care codes from COEL Model v1.0

This note documents a correction made when moving from **COEL Model v1.0** to **COEL Model v2.0**. The change resolves a set of duplicate codes affecting wound care behavioural events in Clusters 2 and 3.

The COEL Model uses a four-level code:

`Cluster.Class.SubClass.Element`

together with a human-readable `Name` to identify each event. Within a given version of the model, the combination `(Cluster, Class, SubClass, Element)` is intended to be unique.

---

## Issue in COEL Model v1.0

In COEL Model v1.0, six wound care behavioural events for children and adults were assigned codes that collided with existing bedtime care behavioural events in Clusters 2 and 3.

### Affected clusters
- Cluster 2: child care
- Cluster 3: adult care

### Problem summary
- Bedtime care behavioural events such as `Kiss child goodnight`, `Tuck childs covers in`, and `Rock child` were placed in `Class 6, SubClass 1`.
- Wound care behavioural events such as `Clean wound for child`, `Apply dressing to child`, and their adult equivalents were also coded in `Class 6, SubClass 1` using overlapping `Element` numbers.
- As a result, some `(Cluster, Class, SubClass, Element)` codes were assigned to two distinct behavioural events, breaking the intended uniqueness of the model code.

### Additional naming inconsistency
In Cluster 3, several wound care behavioural events used text referring to a `child` in the `Name` field despite being adult-focused events.

This created ambiguity whenever COEL codes were used as identifiers in downstream systems, registries, or semantic resources.

---

## Corrections implemented in COEL Model v2.0

COEL Model v2.0 resolves this issue by separating wound care into its own subclass under `Class 7` and standardising the affected names.

### 1. New subclass labels

| Cluster | Class | SubClass | Name |
|---------|-------|----------|------|
| 2 | 7 | 1 | Child wound care |
| 3 | 7 | 1 | Adult wound care |

These subclasses group wound care behavioural events separately from bedtime care.

### 2. Reassignment of wound care elements

All wound care elements previously assigned to `Class 6, SubClass 1` in v1.0 were reassigned to `Class 7, SubClass 1` in v2.0.

#### Child wound care (Cluster 2)

| Old code (v1.0) | New code (v2.0) | Name |
|-----------------|-----------------|------|
| 2.6.1.1 | 2.7.1.1 | Clean wound for child |
| 2.6.1.2 | 2.7.1.2 | Apply dressing to child |
| 2.6.1.3 | 2.7.1.3 | Apply bandage to child |
| 2.6.1.4 | 2.7.1.4 | Change dressing for child |
| 2.6.1.5 | 2.7.1.5 | Change bandage for child |

#### Adult wound care (Cluster 3)

| Old code (v1.0) | New code (v2.0) | Name |
|-----------------|-----------------|------|
| 3.6.1.1 | 3.7.1.1 | Clean wound for adult |
| 3.6.1.2 | 3.7.1.2 | Look for infection in adult |
| 3.6.1.3 | 3.7.1.3 | Apply dressing to adult |
| 3.6.1.4 | 3.7.1.4 | Apply bandage to adult |
| 3.6.1.5 | 3.7.1.5 | Change dressing for adult |

The bedtime care behavioural events remain in `Class 6, SubClass 1` and retain their original codes. The collision between bedtime care and wound care codes is therefore removed.

### 3. Name corrections for adult wound care

In Cluster 3, any wound care element whose `Name` still contained the word `child` was corrected to refer to `adult`. The updated names are reflected in the table above.

### 4. Model invariants after correction

After these corrections:

- each `(Cluster, Class, SubClass, Element)` combination is unique within COEL Model v2.0
- the four-level code can again be treated as a stable identifier for each behavioural event
- only the location and naming of wound care behavioural events in Clusters 2 and 3 changed
- no other clusters or classes were affected

---

## Backward compatibility and migration

For datasets or systems that currently use COEL Model v1.0 codes:

- any record using the v1.0 wound care codes in Clusters 2 and 3 should be remapped according to the tables above
- bedtime care codes in `Class 6, SubClass 1` remain unchanged
- if COEL codes are embedded in downstream schemas or semantic resources, they should be updated to reference the v2.0 wound care codes

Recommended actions:

1. record **COEL Model v2.0** as the active version in registries and documentation
2. explicitly note that the affected v1.0 wound care codes in Clusters 2 and 3 are deprecated and superseded by the v2.0 codes listed here