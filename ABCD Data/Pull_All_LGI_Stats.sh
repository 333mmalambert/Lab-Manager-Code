################################################################################################################################
# Pull_All_LGI_Stats.sh
# Authors: Emma Lambert & Dylan Forrest
# Date: 8/21/26
# Path: /INSERT/PATH/HERE/Pull_All_LGI_Stats.sh
################################################################################################################################

start_time=$(date)
echo $start_time

LH_LGI_File=//INSERT/PATH/HERE/All_6.0_LGI_Stats_LH.csv
RH_LGI_File=/INSERT/PATH/HERE/All_6.0_LGI_Stats_RH.csv

# Set Subjects Directory to where all LGI derivative data is located
SUBJECTS_DIR=/INSERT/PATH/HERE/

cd $SUBJECTS_DIR
echo ""
echo "SUBJECTS_DIR:"
pwd
echo ""
echo "Building List of Subjects..."
echo ""

#########################################################
############# Build List of sub-... Dirs ################
#########################################################

# get list of all directories in the current directory (SUBJECTS_DIR) that are for each individual subject
subjects_list=$(find . -maxdepth 1 -type d -name "sub*" | sort -u)
total_subjects=$(find . -maxdepth 1 -type d -name "sub*" | wc -l)

# shave off "./" from each one
for sub in $subjects_list; do
	ID=$(echo $sub | cut -d'/' -f2)
	subjects_list_clean+="$ID"$'\n'
done

echo "SUBJECTS TO SEARCH:"
#echo $subjects_list_clean
printf "%s" "$subjects_list_clean"
echo ""
echo "$total_subjects Total Subjects"
echo ""

#################################################################
############# Search Through and Pull LGI Values ################
#################################################################

# re-instantiate volumes file
echo "participant_id,Index,SegId,NVertices,Area_mm2,StructName,Mean,StdDev,Min,Max,Range," > $LH_LGI_File
echo "participant_id,Index,SegId,NVertices,Area_mm2,StructName,Mean,StdDev,Min,Max,Range," > $RH_LGI_File

for sub in $subjects_list_clean; do
	
	((subject_num++))
	
	# enter subject's derivative directory
	cd $SUBJECTS_DIR/$sub/stats/
	
	# left hemisphere
	lh_lgi="lh.aparc.pial_lgi.stats"
	tr -s '[:blank:]' ',' <$lh_lgi >lh_temp.txt 
	sed -i "s/^/$sub/" lh_temp.txt
	tail -n 35 lh_temp.txt >> $LH_LGI_File 
	rm lh_temp.txt
	
	# right hemisphere
	rh_lgi="rh.aparc.pial_lgi.stats"
	tr -s '[:blank:]' ',' <$rh_lgi >rh_temp.txt
	sed -i "s/^/$sub/" rh_temp.txt
	tail -n 35 rh_temp.txt >> $RH_LGI_File 
	rm rh_temp.txt
	
done

echo "Congrats Mayor of LGI City"
