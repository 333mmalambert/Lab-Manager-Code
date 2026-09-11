#!/bin/bash

##########################################################################
# Derivative_Cleanup.sh
# Author: Dylan Forrest & Emma Lambert
# Date: 9/10/2026
# Path: INSERT PATH HERE
##########################################################################

# Code is intended for use whenever an error occurs in the initial Batcher.sh pipeline and you want to rerun the batch
	# Participants run in the longitudinal pipeline can't have a directory already existing in derivative directory

# Removes Derivative subject directories for those appearing in 7.0_Prio_niis.txt (Subjects of most recent batch)
	# For review:  Scans from our 7.0_Hippo_Fem_Prio_list.tsv -> 7.0_Prio_niis.txt (run Long_Niftis.sh to get all scans)
	#	       -> Longitudinal_Rebuild_List.txt -> imported into LargeNiftiList.txt (run Batcher.sh to begin long-pipeline)

## WARNING: Every participant listed in 7.0_Prio_niis.txt will be removed from SUBJECTS_DIR  ##

# ok now we code :)


## Define variables
LAST_BATCH="/INSERT/PATH/HERE/7.0_Prio_niis.txt"


## Change directory into SUBJECTS_DIR ##
cd $SUBJECTS_DIR


echo "##################################################"
echo "# Removing Previous Batch's Subject Directories #"
echo "##################################################"


## Remove Subject Directories from SUBJECTS_DIR (currently 7.0) ##
while IFS= read -r scan; do
	subject_id=$(echo "$scan" | cut -d'_' -f1)
	echo "Removing directories for: $subject_id"
	rm -r $subject_id*
	echo ""
	
done < $LAST_BATCH


echo "################################"
echo "# Derivative Directory Cleaned #"
echo "################################"
