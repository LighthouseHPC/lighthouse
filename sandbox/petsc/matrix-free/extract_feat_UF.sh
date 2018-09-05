#!/bin/sh

export CODE_DIR='/home/users/kanikas/RNET_git/MLNeams/petsc_extraction'
export FEAT_DIR='features_extracted_UF'
mkdir -p $FEAT_DIR

export DATA_DIR='/disks/large/shared/soft/UFloridaSparseMat/petsc'
#Using Ben's script (.full) to extract features with matrix-free approach
for i in $DATA_DIR/*.petsc; do
	echo "------------------------------------"
	name=$(echo "$i" | cut -f 8 -d '/')
	out_file=$(echo "$name" | cut -f 1 -d '.')
	echo outFile:$out_file
	echo Whole name: $CODE_DIR/$FEAT_DIR/$name
	$CODE_DIR/./full.a $DATA_DIR/$name ./$FEAT_DIR/$out_file.out 10 10 0 1
	echo $name.out
	echo Going to run: $CODE_DIR/./full.a $DATA_DIR/$name ./$FEAT_DIR/$out_file.out 10 10 0 1
	done
