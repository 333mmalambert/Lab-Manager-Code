#!/bin/bash

##########################################################################
# Long_Niftis.sh
# Author: Emma Lambert; Updated by Dylan Forrest
# Date: 7/10/2026
# Path: /INSERT/PATH/HERE/Long_Niftis.sh
##########################################################################

# Code is intended to pull subject IDs from the 7.0 priority list and create a chronological list of all sessions.
    # In order to segment the hippocampus, participants must have all timepoints re-run through the longitudinal pipeline.

# Output of this code MUST be reviewed in order to remove duplicate sanes (e.g. run-01, run-02, run-03).
   # In the event of duplicate scans, priortize the most RECENT scan as this is typically the least error-prone. 
   # Example:
    # sub-84PUG220_ses-00A_rec-norm_run-01_T1w.nii.gz
    # sub-84PUG220_ses-00A_rec-norm_run-02_T1w.nii.gz
    # Remove the rec-norm_run-01!


cd /INSERT/PATH/HERE/

PRIO_7_LIST=/INSERT/PATH/HERE/7.0_Prio_niis.txt"
ALL_7_LIST="/INSERT/PATH/HERE/7.0_niis.txt"
OUTPUT="/INSERT/PATH/HERE/Longitudinal_Rebuild_List.txt"

# Extract unique subjects IDs from 7.0 priority list
cut -d'_' -f1 "$PRIO_7_LIST" | sort -u | while read sub;
 do
   grep "^${sub}_" "$ALL_7_LIST"
done | sort -u > "$OUTPUT"

echo "Created $OUTPUT"
echo "Total scans: $(wc -l < "$OUTPUT")"
echo "Subjects: $(cut -d'_' -f1 "$OUTPUT" | sort -u | wc -l)"
