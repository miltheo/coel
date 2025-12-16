# COEL Model v2.0

## Correction of duplicate wound care codes from COEL Model v1.0

This note documents a correction made to the COEL Model v1.0 when moving from COEL Model v1.0 to COEL Model v2.0. The change resolves a set of duplicate codes that affected wound care behavioural events in Clusters 2 and 3.

The COEL Model uses a four level code\
`Cluster.Class.SubClass.Element`\
together with a human readable `Name` to identify each event. Within a given version of the model, the combination `(Cluster, Class, SubClass, Element)` is intended to be unique.

------------------------------------------------------------------------

## Issue in COEL Model v1.0

In COEL Model v1.0, six wound care behavioural events for children and adults were assigned codes that collided with existing "putting to bed" behavioural events in Clusters 2 and 3.

-   **Clusters affected**
    -   Cluster 2 (child care)\
    -   Cluster 3 (adult care)
-   **Problem pattern**
    -   Bedtime care behavioural events (for example, "Kiss child goodnight", "Tuck childs covers in", "Rock child") were correctly placed in `Class 6, SubClass 1`.\
    -   Wound care behavioural events such as "Clean wound for child", "Apply dressing to child" and their adult equivalents were also coded under `Class 6, SubClass 1` with overlapping `Element` numbers.\
    -   As a result, some `(Cluster, Class, SubClass, Element)` codes were shared by two distinct behavioural events, which breaks the uniqueness requirement.
-   **Adult naming inconsistency**
    -   In Cluster 3, several wound care behavioural events used text referring to a "child" in the `Name` field, despite being adult focused.

This created ambiguity whenever the COEL codes were used as identifiers in downstream systems, ontologies, or data objects.

------------------------------------------------------------------------

## Corrections implemented in COEL Model v2.0

COEL Model v2.0 corrects this issue by separating wound care into its own subclass under `Class 7` and by standardising the naming.

### 1. New subclass labels

The following subclass names were defined or updated:

| Cluster | Class | SubClass | Name (v2.0)      |
|---------|-------|----------|------------------|
| 2       | 7     | 1        | Child wound care |
| 3       | 7     | 1        | Adult wound care |

These SubClasses group wound care behavioural events separately from bedtime care.

### 2. Reassignment of wound care elements

All wound care elements that previously sat in `Class 6, SubClass 1` in v1.0 have been reassigned to `Class 7, SubClass 1` in v2.0.

**Child wound care (Cluster 2)**

| Old code (v1.0) | New code (v2.0) | Name                      |
|-----------------|-----------------|---------------------------|
| 2.6.1.1         | 2.7.1.1         | Clean wound for child     |
| 2.6.1.2         | 2.7.1.2         | Apply dressing to child   |
| 2.6.1.3         | 2.7.1.3         | Apply bandage to child    |
| 2.6.1.4         | 2.7.1.4         | Change dressing for child |
| 2.6.1.5         | 2.7.1.5         | Change bandage for child  |

**Adult wound care (Cluster 3)**

| Old code (v1.0) | New code (v2.0) | Name                        |
|-----------------|-----------------|-----------------------------|
| 3.6.1.1         | 3.7.1.1         | Clean wound for adult       |
| 3.6.1.2         | 3.7.1.2         | Look for infection in adult |
| 3.6.1.3         | 3.7.1.3         | Apply dressing to adult     |
| 3.6.1.4         | 3.7.1.4         | Apply bandage to adult      |
| 3.6.1.5         | 3.7.1.5         | Change dressing for adult   |

The bedtime care behavioural events remain in `Class 6, SubClass 1` and retain their original codes. The collision between bedtime care and wound care codes is therefore removed.

### 3. Name corrections for adult wound care

In Cluster 3, any wound care element whose `Name` still contained the word "child" has been corrected to refer to "adult". The updated names are reflected in the table above.

### 4. Model invariants

After these corrections:

-   Each `(Cluster, Class, SubClass, Element)` combination is unique within COEL Model v2.0.\
-   The four level code can again be treated as a stable identifier for each behavioural event.\
-   Only the location and naming of wound care behavioural events in Clusters 2 and 3 have changed. No other clusters or classes are affected.

------------------------------------------------------------------------

## Backward compatibility and migration

For datasets or systems that currently use COEL Model v1.0 codes:

-   Any record that used the v1.0 wound care codes in Clusters 2 and 3 should be remapped according to the tables above.\
-   Bedtime care codes in `Class 6, SubClass 1` remain unchanged, so existing mappings for those behavioural events remain valid.\
-   If COEL codes are embedded in downstream schemas (for example, JSON or JSON LD representations) they should be updated to reference the v2.0 wound care codes.

It is recommended to:

1.  Record COEL Model v2.0 as the active version in registries and documentation.\
2.  Explicitly note that the v1.0 wound care codes in Clusters 2 and 3 are deprecated and superseded by the v2.0 codes listed here.

------------------------------------------------------------------------

## Files

-   `COEL Model v2.0.json`\
    COEL Model v2.0 JSON representation, incorporating the corrections described above.

If you have questions about this change or encounter unexpected codes in legacy data, please refer to this mapping or contact the maintainers of the COELITION OASIS standard.
