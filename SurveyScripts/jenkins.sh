#!/bin/bash

# Script launched by Jenkins to do the full survey.
set -xe

cd ..
TOPDIR=`pwd`
git clean -fxd

WORKINGNAME=ghostbuster_survey_`date +'%s'`
LOCALDIR="$HOME/local/working_copies_temp/$WORKINGNAME"
mkdir -p "$LOCALDIR"
time rsync -vrplt --delete "$TOPDIR/" "$LOCALDIR/"
cd "$LOCALDIR/SurveyScripts/"

# Just in case we are run in a dirty directory:
make clean

# First, set up directories:
# --------------------------------------------------------------------------------

# Try two places:
localstorage=/home.local/$USER/
if ! [ -d $localstorage ]; then
    localstorage=$HOME/local/
fi

if ! [ -d $localstorage ]; then
    echo "Local storage not found."
    exit 1
fi

# input
# ------------------------------

origdir=$localstorage/hackage_all_tarballs/
mkdir -p ./data/

# IU/cutter specific hack.  Get the data:
if ! [ -d "$origdir" ]; then
    rsync -rplt crest-team@cutter.crest.iu.edu:/home.local/crest-team/hackage_all_tarballs/ "$origdir"
fi

inputdir=./data/0_hackage_all_tarballs

if [ "$SKIPTO" == "" ]; then
    # Run the WHOLE data set.
    ln -s -f $origdir $inputdir
else
    echo "Running on a SUBRANGE of input packages.  "
    ls $origdir/ | sort > full_file_list.txt
    echo "The FULL data set discovered has this many tarballs:"`wc -l full_file_list.txt`

    # Annoyingly, tail is zero-based
    onebased=$((SKIPTO + 1))
    tail -n+${onebased} full_file_list.txt > all_following.txt
    if [ "$RUNONLY" == "" ]; then
        cp all_following.txt my_chunk.txt
    else
        head -n${RUNONLY} all_following.txt > my_chunk.txt
    fi

    rm -rf $inputdir
    mkdir -p $inputdir
    echo "Here's the size of my_chunk.txt: "`wc -l my_chunk.txt`
    for file in `cat my_chunk.txt`; do
        echo "Linking: $file"
        ln -s "$origdir/$file" "$inputdir/$file"
    done
fi


# intermediates
# ------------------------------

scratch=$localstorage/GhostbusterSurvey/
mkdir -p $scratch

intermediates="1_only_newest_versions 2_untarred 3_ddef_clusters 4_compiled_ddefs"
for dir in $intermediates; do
    mkdir -p $scratch/$dir
    ln -s -f $scratch/$dir ./data/$dir
done

# final output location
# ------------------------------

if [ "$BUILD_NUMBER" == "" ]; then
    BUILD_NUMBER="unknown"
fi

# outdir=$HOME/ghostbuster_survey_collected_output_stats/run_`date +"%s"`/
builddir=build_${BUILD_NUMBER}/skipto_${SKIPTO}_time_`date +"%s"`/
outdir=$HOME/ghostbuster_survey_collected_output_stats/$builddir
metadata="$outdir/collection_machine_info.txt"

mkdir -p "$outdir"
uname -a | tee $metadata
echo "" >> "$metadata"
echo "Collected on `date` " >> "$metadata"
echo "Collected on machine `hostname` " >> "$metadata"
echo "Collected from working copy `pwd` " >> "$metadata"

# Next, run the pipeline:
# --------------------------------------------------------------------------------

time make all

# We *could* do this collection in between steps...

find "./data/3_ddef_clusters/"  -name "*.csv" > all_csvs.txt
find "./data/4_compiled_ddefs/" -name "*.csv" >> all_csvs.txt
n=0
for csvfile in `cat all_csvs.txt`; do
    echo "Filing away: $csvfile"
    cp "$csvfile" "$outdir/$((n++))_"`basename $csvfile`
    csvstat $csvfile || echo ok
done

find "./data/4_compiled_ddefs/" -name "*.log" >> logfiles.txt
set +x
echo "Begin copying log files."
time for logfile in `cat logfiles.txt`; do
    dest="$outdir/$logfile"
    mkdir -p `dirname "$dest"`
    cp "$logfile" "$dest"
done
echo "Done copying log files."

set +e

echo "Now phone home by sending all results to cutter:"
remotepath=local/ghostbuster_survey_data_bak/magramal_all_results/$builddir
ssh crest-team@cutter.crest.iu.edu mkdir -p "$remotepath/data/4_compiled_ddefs"
ssh crest-team@cutter.crest.iu.edu mkdir -p "$remotepath/data/3_ddef_clusters"
time rsync -plt "./data/4_compiled_ddefs/" crest-team@cutter.crest.iu.edu:"$remotepath/data/4_compiled_ddefs/"
time rsync -plt "./data/3_ddef_clusters/" crest-team@cutter.crest.iu.edu:"$remotepath/data/3_ddef_clusters"
