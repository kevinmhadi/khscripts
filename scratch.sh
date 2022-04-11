cd /gpfs/commons/groups/imielinski_lab/data/FAHNSC/Flow/emerald/Emerald3/AS3T/align

outputdir=align
ok=$(ls ${outputdir}/ema-bin-* | grep -v ".bam$" | tr "\n" " ")

alignbin=""
for bin in $ok
do
     { samtools quickcheck ${bin}.bam && echo "${bin} aligned" ||
	     { echo "${bin} not aligned yet" && alignbin="$alignbin $bin"; }; } 2> /dev/null
done



{ echo "bla"
  echo "bloo" \
      ; }


get_fn_sans_ext() {
    filename=$(basename -- "${1}")
    extension="${filename##*.}"
    filename="${filename%.*}"
    # echo 'vcf_ext=${extension}'
    echo ${filename}
}

fin_fn=$(get_fn_sans_ext ${OUTPUT})

test -s ${OUTPUT} && present1=true || present1=false

present2=true
if [ "$SOMATIC" = "TRUE" ]; then
    { test -s ${OUTPUT}.filtered.somatic.sv.vcf.bgz &&
	test -s ${OUTPUT}.unfiltered.somatic.sv.vcf.bgz &&
	test -s ${OUTPUT}.gripss.filtered.somatic.vcf &&
	present2=true; } ||
	present2=false;
fi

{ $present1 && $present2 && echo "All outputs present... FINISHED"; } ||
    { echo "Something went wrong... exiting" && exit 1; }

{ $present && echo "skipping IdentifyVariants"; } ||
    { echo "" && echo $cmd; } &&
	{ echo "worked?" && broke=true || broke=false; } 1>> $logfile &&
	echo "okok"

broke=false
present=false

if [[ $do_call == true ]] ; then
	write_status "Start calling	$output_vcf"
	if [[ "$jobnodes" != "1" ]] ; then
		write_status "Error: variant calling does not (yet) support multiple nodes for a given input file."
		exit $EX_USAGE
	fi
	if [[ ! -f $output_vcf ]] ; then
		dir=$workingdir/$(basename $output_vcf).gridss.working
		prefix=$dir/$(basename $output_vcf)
		mkdir -p $dir
		if [[ ! -d $dir ]] ; then
			write_status "Unable to create directory $dir"
			exit $EX_CANTCREAT
		fi
		write_status "Running	IdentifyVariants	$output_vcf"
		{ test -f $prefix.unallocated.vcf ||
		      test -f $prefix.allocated.vcf \
		  ; } && present=true || present=false
		cmd="$timecmd java -Xmx$jvmheap $jvm_args \
				-Dgridss.output_to_temp_file=true \
				-cp $gridss_jar gridss.IdentifyVariants \
				TMP_DIR=$workingdir \
				WORKING_DIR=$workingdir \
				REFERENCE_SEQUENCE=$reference \
				WORKER_THREADS=$threads \
				$input_args \
				$blacklist_arg \
				$config_args \
				ASSEMBLY=$assembly \
				OUTPUT_VCF=$prefix.unallocated.vcf \
				$readpairing_args"
		trap 'rm $prefix.unallocated.vcf' 0 1 2 3 6 9 15
		{ $present && echo "skipping IdentifyVariants"; } ||
		    { echo "" && echo $cmd; } &&
			{ eval $cmd && broke=true || broke=false; } 1>&2 2>> $logfile
		$broke && rm $prefix.unallocated.vcf && exit 1
		write_status "Running	AnnotateVariants	$output_vcf"
		test -f $prefix.allocated.vcf &&
		    present=true || present=false
		cmd="$timecmd java -Xmx$jvmheap $jvm_args \
				-Dgridss.output_to_temp_file=true \
				-Dgridss.async.buffersize=2048 \
				-cp $gridss_jar gridss.AnnotateVariants \
				TMP_DIR=$workingdir \
				WORKING_DIR=$workingdir \
				REFERENCE_SEQUENCE=$reference \
				WORKER_THREADS=$threads \
				$input_args \
				$blacklist_arg \
				$config_args \
				ASSEMBLY=$assembly \
				INPUT_VCF=$prefix.unallocated.vcf \
				OUTPUT_VCF=$prefix.allocated.vcf \
				$picardoptions \
				$readpairing_args"
		trap 'rm $prefix.allocated.vcf' 1 2 3 6 9 15
		{ $present && echo "skipping AnnotateVariants"; } ||
		    { echo "" && echo $cmd; } &&
			{ eval $cmd && broke=true || broke=false; } 1>&2 2>> $logfile
		$broke && rm $prefix.allocated.vcf && exit 1
		# { $timecmd java -Xmx$jvmheap $jvm_args \
		# 		-Dgridss.output_to_temp_file=true \
		# 		-Dgridss.async.buffersize=2048 \
		# 		-cp $gridss_jar gridss.AnnotateVariants \
		# 		TMP_DIR=$workingdir \
		# 		WORKING_DIR=$workingdir \
		# 		REFERENCE_SEQUENCE=$reference \
		# 		WORKER_THREADS=$threads \
		# 		$input_args \
		# 		$blacklist_arg \
		# 		$config_args \
		# 		ASSEMBLY=$assembly \
		# 		INPUT_VCF=$prefix.unallocated.vcf \
		# 		OUTPUT_VCF=$prefix.allocated.vcf \
		# 		$picardoptions \
		# 		$readpairing_args \
		# ; } 1>&2 2>> $logfile
		write_status "Running	AnnotateInsertedSequence	$output_vcf"
		repeatmaskerbed_cmdline=""
		if [[ "$repeatmaskerbed" != "" ]] ; then
			repeatmaskerbed_cmdline="REPEAT_MASKER_BED=$repeatmaskerbed"
		fi
		trap 'rm $prefix.allocated.vcf' 1 2 3 6 9 15
		cmd="$timecmd java -Xmx4g $jvm_args \
				-Dgridss.output_to_temp_file=true \
				-cp $gridss_jar gridss.AnnotateInsertedSequence \
				TMP_DIR=$workingdir \
				WORKING_DIR=$workingdir \
				REFERENCE_SEQUENCE=$reference \
				WORKER_THREADS=$threads \
				INPUT=$prefix.allocated.vcf \
				OUTPUT=$output_vcf \
				$repeatmaskerbed_cmdline \
				$picardoptions"
		test "$externalaligner" == "true" &&
		    cmd="$cmd ${aligner_args_bwa}"
		test -f $output_vcf && present=true || present=false
		trap 'rm $output_vcf' 1 2 3 6 9 15
		{ $present && echo "skipping AnnotateInsertedSequence"; } ||
		    { echo "" && echo $cmd; } &&
			{ eval $cmd && broke=true || broke=false; } 1>&2 2>> $logfile
		$broke && rm $output_vcf && exit 1
		# { $timecmd java -Xmx4g $jvm_args \
		# 		-Dgridss.output_to_temp_file=true \
		# 		-cp $gridss_jar gridss.AnnotateInsertedSequence \
		# 		TMP_DIR=$workingdir \
		# 		WORKING_DIR=$workingdir \
		# 		REFERENCE_SEQUENCE=$reference \
		# 		WORKER_THREADS=$threads \
		# 		INPUT=$prefix.allocated.vcf \
		# 		OUTPUT=$output_vcf \
		# 		$repeatmaskerbed_cmdline \
		# 		$picardoptions \
		# && $rmcmd $prefix.allocated.vcf \
		# ; } 1>&2 2>> $logfile
		test -f ${output_vcf} && $rmcmd $prefix.unallocated.vcf &&
		    $rmcmd $prefix.allocated.vcf
	else
		write_status  "Skipping variant calling	$output_vcf"
	fi
	write_status "Complete calling	$output_vcf"
fi


broke=false
present=false

if [[ $do_call == true ]] ; then
	write_status "Start calling	$output_vcf"
	if [[ "$jobnodes" != "1" ]] ; then
		write_status "Error: variant calling does not (yet) support multiple nodes for a given input file."
		exit $EX_USAGE
	fi
	if [[ ! -f $output_vcf ]] ; then
		dir=$workingdir/$(basename $output_vcf).gridss.working
		prefix=$dir/$(basename $output_vcf)
		mkdir -p $dir
		if [[ ! -d $dir ]] ; then
			write_status "Unable to create directory $dir"
			exit $EX_CANTCREAT
		fi
		
		write_status "Running	IdentifyVariants	$output_vcf"
		{ test -f $prefix.unallocated.vcf ||
		      test -f $prefix.allocated.vcf \
		  ; } && present=true || present=false
		cmd="$timecmd java -Xmx$jvmheap $jvm_args \
				-Dgridss.output_to_temp_file=true \
				-cp $gridss_jar gridss.IdentifyVariants \
				TMP_DIR=$workingdir \
				WORKING_DIR=$workingdir \
				REFERENCE_SEQUENCE=$reference \
				WORKER_THREADS=$threads \
				$input_args \
				$blacklist_arg \
				$config_args \
				ASSEMBLY=$assembly \
				OUTPUT_VCF=$prefix.unallocated.vcf \
				$readpairing_args"
		trap 'rm $prefix.unallocated.vcf' 1 2 3 6 9 15
		{ $present && echo "skipping IdentifyVariants"; } ||
		    { echo "" && echo $cmd; } &&
			{ eval $cmd && broke=false || broke=true; } 1>&2 2>> $logfile
		$broke && rm $prefix.unallocated.vcf && exit 1
		
		write_status "Running	AnnotateVariants	$output_vcf"
		test -f $prefix.allocated.vcf &&
		    present=true || present=false
		cmd="$timecmd java -Xmx$jvmheap $jvm_args \
				-Dgridss.output_to_temp_file=true \
				-Dgridss.async.buffersize=2048 \
				-cp $gridss_jar gridss.AnnotateVariants \
				TMP_DIR=$workingdir \
				WORKING_DIR=$workingdir \
				REFERENCE_SEQUENCE=$reference \
				WORKER_THREADS=$threads \
				$input_args \
				$blacklist_arg \
				$config_args \
				ASSEMBLY=$assembly \
				INPUT_VCF=$prefix.unallocated.vcf \
				OUTPUT_VCF=$prefix.allocated.vcf \
				$picardoptions \
				$readpairing_args"
		trap 'rm $prefix.allocated.vcf' 1 2 3 6 9 15
		{ $present && echo "skipping AnnotateVariants"; } ||
		    { echo "" && echo $cmd; } &&
			{ eval $cmd && broke=false || broke=true; } 1>&2 2>> $logfile
		$broke && rm $prefix.allocated.vcf && exit 1
		
		write_status "Running	AnnotateInsertedSequence	$output_vcf"
		repeatmaskerbed_cmdline=""
		if [[ "$repeatmaskerbed" != "" ]] ; then
			repeatmaskerbed_cmdline="REPEAT_MASKER_BED=$repeatmaskerbed"
		fi
		trap 'rm $prefix.allocated.vcf' 1 2 3 6 9 15
		cmd="$timecmd java -Xmx4g $jvm_args \
				-Dgridss.output_to_temp_file=true \
				-cp $gridss_jar gridss.AnnotateInsertedSequence \
				TMP_DIR=$workingdir \
				WORKING_DIR=$workingdir \
				REFERENCE_SEQUENCE=$reference \
				WORKER_THREADS=$threads \
				INPUT=$prefix.allocated.vcf \
				OUTPUT=$output_vcf \
				$repeatmaskerbed_cmdline \
				$picardoptions"
		test "$externalaligner" == "true" &&
		    cmd="$cmd ${aligner_args_bwa}"
		test -f $output_vcf && present=true || present=false
		trap 'rm $output_vcf' 1 2 3 6 9 15
		{ $present && echo "skipping AnnotateInsertedSequence"; } ||
		    { echo "" && echo $cmd; } &&
			{ eval $cmd && broke=false || broke=true; } 1>&2 2>> $logfile
		$broke && rm $output_vcf && exit 1
		test -f ${output_vcf} && $rmcmd $prefix.unallocated.vcf &&
		    $rmcmd $prefix.allocated.vcf
	else
		write_status  "Skipping variant calling	$output_vcf"
	fi
	write_status "Complete calling	$output_vcf"
fi




refdir

mkdir -p tmpref

str="Copyng reference to temporary directory"
test ! -f tmpref/$(basename ${REFERENCE}) && echo "copying ${REFERENCE} to ./tmpref/" && cp tmpref/$(basename ${REFERENCE}) tmpref/
test ! -f tmpref/$(basename ${REFERENCE}).fai && echo "copying ${REFERENCE.fai} to ./tmpref/" && cp tmpref/$(basename ${REFERENCE}).fai tmpref/
test ! -f tmpref/$(basename ${REFERENCE}).bwt && "copying ${REFERENCE.bwt} to ./tmpref/" && cp tmpref/$(basename ${REFERENCE}).bwt tmpref/
{ test ! -f tmpref/$(basename ${REFERENCE}).img && "copying ${REFERENCE.img} to ./tmpref/" && cp tmpref/$(basename ${REFERENCE}).bwt tmpref/ ; } || { echo "img not found" && echo "please run "; }

jvm_args="-Dgridss.keepTempFiles=true"
jvm_args="$jvm_args \
	-Dreference_fasta=$REFERENCE \
	-Dsamjdk.use_async_io_read_samtools=true \
	-Dsamjdk.use_async_io_write_samtools=true \
	-Dsamjdk.use_async_io_write_tribble=true \
	-Dsamjdk.buffer_size=4194304"

"java -Xmx4g $jvm_args \
		-cp $GRIDSS_JAR gridss.PrepareReference \
		REFERENCE_SEQUENCE=$REFERENCE"

REFERENCE="./tmpref/$(basename ${REFERENCE})"


{ $present1 && $present2 &&
      echo "Successfully called, removing temporary files" &&
      rm *.bam &&
      rm -rf *.working &&
      rm -rf tmpref/*
      echo "All outputs present... FINISHED" \
	  ; } ||
    { echo "Something went wrong... exiting" && exit 1; }


export cmd="{ echo \"bla\" && echo \"blap\"; }";
/usr/bin/time -p sh -c 'echo "$(date): $cmd" && eval $cmd'




write_status "Running	CollectGridssMetricsAndExtractSVReads|samtools	$f"


cmd="$timecmd java -Xmx4g $jvm_args \
	   -cp $gridss_jar gridss.CollectGridssMetricsAndExtractSVReads \
	   TMP_DIR=$dir \
	   ASSUME_SORTED=true \
	   I=$f \
	   O=$prefix \
	   THRESHOLD_COVERAGE=$maxcoverage \
	   FILE_EXTENSION=null \
	   GRIDSS_PROGRAM=null \
	   GRIDSS_PROGRAM=CollectCigarMetrics \
	   GRIDSS_PROGRAM=CollectMapqMetrics \
	   GRIDSS_PROGRAM=CollectTagMetrics \
	   GRIDSS_PROGRAM=CollectIdsvMetrics \
	   GRIDSS_PROGRAM=ReportThresholdCoverage \
	   PROGRAM=null \
	   PROGRAM=CollectInsertSizeMetrics \
	   SV_OUTPUT=/dev/stdout \
	   COMPRESSION_LEVEL=0 \
	   METRICS_OUTPUT=$prefix.sv_metrics \
	   INSERT_SIZE_METRICS=$tmp_prefix.insert_size_metrics \
	   $readpairing_args \
	   UNMAPPED_READS=false \
	   MIN_CLIP_LENGTH=5 \
	   INCLUDE_DUPLICATES=true \
	   $picardoptions \
      | $timecmd samtools sort \
		 -n \
		 -T $tmp_prefix.namedsorted-tmp \
		 -Obam \
		 -o $tmp_prefix.namedsorted.bam \
		 -@ $threads \
		 /dev/stdin"



{ $timecmd java -Xmx4g $jvm_args \
	   -cp $gridss_jar gridss.CollectGridssMetricsAndExtractSVReads \
	   TMP_DIR=$dir \
	   ASSUME_SORTED=true \
	   I=$f \
	   O=$prefix \
	   THRESHOLD_COVERAGE=$maxcoverage \
	   FILE_EXTENSION=null \
	   GRIDSS_PROGRAM=null \
	   GRIDSS_PROGRAM=CollectCigarMetrics \
	   GRIDSS_PROGRAM=CollectMapqMetrics \
	   GRIDSS_PROGRAM=CollectTagMetrics \
	   GRIDSS_PROGRAM=CollectIdsvMetrics \
	   GRIDSS_PROGRAM=ReportThresholdCoverage \
	   PROGRAM=null \
	   PROGRAM=CollectInsertSizeMetrics \
	   SV_OUTPUT=/dev/stdout \
	   COMPRESSION_LEVEL=0 \
	   METRICS_OUTPUT=$prefix.sv_metrics \
	   INSERT_SIZE_METRICS=$tmp_prefix.insert_size_metrics \
	   $readpairing_args \
	   UNMAPPED_READS=false \
	   MIN_CLIP_LENGTH=5 \
	   INCLUDE_DUPLICATES=true \
	   $picardoptions \
      | $timecmd samtools sort \
		 -n \
		 -T $tmp_prefix.namedsorted-tmp \
		 -Obam \
		 -o $tmp_prefix.namedsorted.bam \
		 -@ $threads \
		 /dev/stdin \
  ; } 1>&2 2>> $logfile


REFERENCE=/gpfs/commons/groups/imielinski_lab/DB/GATK/human_g1k_v37_with_chr.fasta
REFERENCE="bla"
blacklist="$( cat ~/DB/modules/GRIDSS/gridss_ref_blacklist.txt | \
    awk -v var=$(readlink -f $REFERENCE) '$1 == var {print $2}' | xargs -I {} readlink -f {})"

ent="$( cat ~/DB/modules/GRIDSS/gridss_ref_blacklist.txt | \
    awk -v var=$(readlink -f $REFERENCE) '$1 == var')"

echo "$ent" | awk '{print $1}'

cat ${REFERENCE}.fai | awk '{print $1}' | uniq > rname
cat ${BLACKLIST} | awk '{print $1}' | uniq > bname

allbninrn=$(Rscript -e "cat(tolower(all(readLines('bname') %in% readLines('rname'))))")

# test "$(cat ${REFERENCE}.fai | awk '{print $1}' | uniq)" == "$(cat ${BLACKLIST} | awk '{print $1}' | uniq)" && echo "ok"

# test "$(cat ${REFERENCE}.fai | awk '{print $1}' | uniq | xargs | sort)" == "$(cat ${BLACKLIST} | awk '{print $1}' | uniq | xargs | sort)" && echo "ok"

# $(readlink -f ~/DB/GATK/human_g1k_v37_with_chr.fasta)
# REFERENCE="
# "~/DB/modules/GRIDSS/gridss_ref_blacklist.txt"

for bam in *.bam; do
    echo $(readlink -f $bam)
    samtools view -H $bam | grep "SN" | awk '{gsub(/^SN:|^ID:/, "", $2); print $2}' > bamrn
    bammatch=$(Rscript -e "cat(tolower(all(readLines('bamrn') == readLines('rname'))))")
    $bammatch && echo "Passed: $bam: matches ${REFERENCE}" || {
	    echo "Failed comparison" &&
            echo "Comparison of the reference name and bamname:" &&
	    echo -e "${REFERENCE}\t$(readlink -f ${bam})" &&
	    echo -e "${REFERENCE}\t$(readlink -f ${bam})" > ./$(basename ${bam})_ref_mismatch
	    paste bamrn rname >> ./$(basename ${bam})_ref_mismatch \
		; }
done

samtools view -H ctK*.bam | grep "SN" | awk '{gsub(/^SN:|^ID:/, "", $2); print $2}'



samtools view -H $bam | awk '$2 ~ /^SN/ {print $0}' | awk '{gsub(/^SN:|^ID:/, "", $2); print $2}' > bamrn



lftp ftp.ncbi.nih.gov:/genomes/genbank/vertebrate_mammalian/Homo_sapiens
echo -e "bla  foobar\nbla" | sed "s/ \t\r\n,:|/ /g" | xargs


if ! { echo $cmd && fuck; }; then echo "samtools flagstat broke"; exit 1; fi

Rscript $@
## if ^^ breaks, then $? will be non-zero
[ ! "$?" == "0" ] && { echo "HRDetect failed"; exit 1; }

if ! { bcftools view $output_vcf 1> /dev/null; }; then
    if ! { echo "$(date): ${cmd}" && eval $cmd; }; then
	rm -rf $output_vcf; exit 1;
    fi
else
    write_status "skipping AnnotateInsertedSequence"
fi

bcftools view $prefix.unallocated.vcf 1> /dev/null && idvdone=true || idvdone=false
bcftools view $prefix.allocated.vcf 1> /dev/null && anvdone=true || anvdone=false

if ! ${idvdone}; then
    if ! { echo "$(date): ${cmd}" && eval $cmd; }; then
	rm -rf $prefix.unallocated.vcf; echo "IdentifyVariants broke!"; exit 1;
    fi
else
    echo "skipping IdentifyVariants"
fi


if ! ${anvdone}; then
    if ! { echo "$(date): ${cmd}" && eval $cmd; }; then
	rm -rf $prefix.allocated.vcf; echo "AnnotateVariants broke!"; exit 1;
    fi
else
    echo "skipping AnnotateVariants"
fi



if ! { $present || $present2; }; then
    cmd="${lib_dir}/bwa mem -M -R \"@RG\tID:${laneid}\tSM:${sampleName}\tPL:ILLUMINA\" -t $NSLOTS ${ref_fasta} ${fq1} ${fq2} | ${lib_dir}/samblaster -M -i /dev/stdin | samtools sort -T ${tmpdir} -@ $sortthread -O bam -l 0 -m ${sortmem}G -o ${tmpName}.aligned_bwa_markdups.bam -"
    if { echo -e "Running alignment, mark dup, sorting:\n$cmd" && eval $cmd; }; then
	echo "Finished alignment!"
    else
	echo "ERROR!! Broke at bwa mem alignment!"; exit 1
    fi
else
    echo "Alignment output already present for ID: ${laneid}, SM: ${sampleName}"
fi


cp -r ~/lab/git/mskilab/gTrack/. ./
git clone ~/lab/git/mskilab/gTrack/. ./

for remote in `git branch -r | grep -v \> | xargs`; do git branch --track ${remote#origin/} $remote; done
git branch -r | grep -v '\->' | while read remote; do git branch --track ${remote#origin/} $remote; done

git remote set-url origin git@github.com:mskilab/gTrack.git
git remote set-url origin https://github.com/mskilab/gTrack.git




cmd='$timecmd java -Xmx4g $jvm_args \
			   -cp $gridss_jar gridss.CollectGridssMetricsAndExtractSVReads \
			   TMP_DIR=$dir \
			   ASSUME_SORTED=true \
			   I=$f \
			   O=$prefix \
			   THRESHOLD_COVERAGE=$maxcoverage \
			   FILE_EXTENSION=null \
			   GRIDSS_PROGRAM=null \
			   GRIDSS_PROGRAM=CollectCigarMetrics \
			   GRIDSS_PROGRAM=CollectMapqMetrics \
			   GRIDSS_PROGRAM=CollectTagMetrics \
			   GRIDSS_PROGRAM=CollectIdsvMetrics \
			   GRIDSS_PROGRAM=ReportThresholdCoverage \
			   PROGRAM=null \
			   PROGRAM=CollectInsertSizeMetrics \
			   SV_OUTPUT=/dev/stdout \
			   COMPRESSION_LEVEL=0 \
			   METRICS_OUTPUT=$prefix.sv_metrics \
			   INSERT_SIZE_METRICS=$tmp_prefix.insert_size_metrics \
			   $readpairing_args \
			   UNMAPPED_READS=false \
			   MIN_CLIP_LENGTH=5 \
			   INCLUDE_DUPLICATES=true \
			   $picardoptions \
		      \| $timecmd samtools sort \
				 -n \
				 -T $tmp_prefix.namedsorted-tmp \
				 -Obam \
				 -o $tmp_prefix.namedsorted.bam \
				 -@ $threads \
				 /dev/stdin'


tmpdir=""
tmpdir="/nfs/"
maketmpdir='if [ "$tmpdir" ]; then TMP_DIR=$tmpdir; echo "making TMP_DIR=$tmpdir"; else TMP_DIR=$dir;  echo "making TMP_DIR=$dir"; fi'
maketmpdir='if [ "$tmpdir" ]; then TMP_DIR=$tmpdir; write_status "making TMP_DIR=$tmpdir"; else TMP_DIR=$dir; write_status "making TMP_DIR=$dir"; fi'

eval $maketmpdir


# eval echo $maketmpdir

cmd='$timecmd java -Xmx4g $jvm_args
			   -cp $gridss_jar gridss.CollectGridssMetricsAndExtractSVReads
			   TMP_DIR=$dir
			   ASSUME_SORTED=true
			   I=$f
			   O=$prefix
			   THRESHOLD_COVERAGE=$maxcoverage
			   FILE_EXTENSION=null
			   GRIDSS_PROGRAM=null
			   GRIDSS_PROGRAM=CollectCigarMetrics
			   GRIDSS_PROGRAM=CollectMapqMetrics
			   GRIDSS_PROGRAM=CollectTagMetrics
			   GRIDSS_PROGRAM=CollectIdsvMetrics
			   GRIDSS_PROGRAM=ReportThresholdCoverage
			   PROGRAM=null
			   PROGRAM=CollectInsertSizeMetrics
			   SV_OUTPUT=/dev/stdout
			   COMPRESSION_LEVEL=0
			   METRICS_OUTPUT=$prefix.sv_metrics
			   INSERT_SIZE_METRICS=$tmp_prefix.insert_size_metrics
			   $readpairing_args
			   UNMAPPED_READS=false
			   MIN_CLIP_LENGTH=5
			   INCLUDE_DUPLICATES=true
			   $picardoptions \| samtools view'

eval echo $cmd | xargs

bla=$(echo $cmd | xargs | xargs)


cmd='echo "$bla"'


export cmd='for accession in 1 2 3 4 5; do
    sh ~/dummy.sh $accession foo;
done'

ok="$(echo "$bla") $(echo 'TMP_DIR=$dir')"

blip="fii"
ok='\{ echo \ "$blip foob" \| sed "s/i/o/g"\; echo "finish"\; \}'

echo $cmd
echo "$cmd"
eval echo $cmd
eval echo "$cmd"
eval "echo '$cmd'"
eval "echo $cmd | xargs | xargs"
eval "$(echo $cmd | xargs | xargs)"



tmpdir=""
tmpdir="/nfs/scratch"
maketmpdir='if [ "$tmpdir" ]\; then TMP_DIR=$tmpdir\; echo "making TMP_DIR=$tmpdir"\; else TMP_DIR=$dir\; echo "making TMP_DIR=$dir"\; fi'

wdir=""
wdir="/nfs/scratch"
makewdir='if [ "$tmpdir" ]\; then TMP_DIR=$tmpdir\; echo "making TMP_DIR=$tmpdir"\; else TMP_DIR=$dir\; echo "making TMP_DIR=$dir"\; fi'
eval "parse $maketmpdir"
eval "$(parse $maketmpdir)"
eval "echo $maketmpdir"
eval eval "$(echo $maketmpdir)"

maketmpdir='if [ "$tmpdir" ]; then TMP_DIR=$tmpdir; else TMP_DIR=$dir; fi'


blip="fii"
ok='{ echo \ "$blip foob" \| sed "s/i/o/g"\; echo "finish"\; \}'
ok='echo \ "$blip foob" \| sed "s/i/o/g"\; echo "finish"'

parse() {
    echo "$@" | xargs | xargs
}
# eval parse "$ok"
# eval "$(parse \"$ok\")"
# eval $(parse "$ok")
eval "parse $ok"
eval "$(parse $ok)"



eval "echo $ok | xargs | xargs"
## executes
eval "$(echo $ok | xargs | xargs)"


collectgridssmetrics='{ $timecmd java -Xmx4g $jvm_args \
	   -cp $gridss_jar gridss.analysis.CollectGridssMetrics \
	   TMP_DIR=$dir \
	   ASSUME_SORTED=true \
	   I=$f \
	   O=$tmp_prefix \
	   THRESHOLD_COVERAGE=$maxcoverage \
	   FILE_EXTENSION=null \
	   GRIDSS_PROGRAM=null \
	   PROGRAM=null \
	   PROGRAM=CollectInsertSizeMetrics \
	   STOP_AFTER=$metricsrecords \
	   $picardoptions \
  \; \} 1\>\&2 2\>\> $logfile'

eval "parse $collectgridssmetrics"
eval "$(parse $collectgridssmetrics)"

computesamtags='{ $timecmd java -Xmx4g $jvm_args \
	   -cp $gridss_jar gridss.ComputeSamTags \
	   TMP_DIR=$dir \
	   WORKING_DIR=$workingdir \
	   REFERENCE_SEQUENCE=$reference \
	   COMPRESSION_LEVEL=0 \
	   I=$tmp_prefix.namedsorted.bam \
	   O=/dev/stdout \
	   RECALCULATE_SA_SUPPLEMENTARY=true \
	   SOFTEN_HARD_CLIPS=true \
	   FIX_MATE_INFORMATION=true \
	   FIX_DUPLICATE_FLAG=true \
	   FIX_SA=true \
	   FIX_MISSING_HARD_CLIP=true \
	   TAGS=null \
	   TAGS=NM \
	   TAGS=SA \
	   TAGS=R2 \
	   TAGS=Q2 \
	   TAGS=MC \
	   TAGS=MQ \
	   ASSUME_SORTED=true \
	   $picardoptions \
      \| $timecmd samtools sort \
		 -T $tmp_prefix.coordinate-tmp \
		 -Obam \
		 -o $tmp_prefix.coordinate.bam \
		 -@ $threads \
		 /dev/stdin \
  \; \} 1\>\&2 2\>\> $logfile\'
eval "parse $computesamtags"
eval "$(parse $computesamtags)"

softcliptstosplitreads='{ $timecmd java -Xmx4g $jvm_args \
	   -Dsamjdk.create_index=false \
	   -cp $gridss_jar gridss.SoftClipsToSplitReads \
	   TMP_DIR=$workingdir \
	   WORKING_DIR=$workingdir \
	   REFERENCE_SEQUENCE=$reference \
	   I=$tmp_prefix.coordinate.bam \
	   O=$tmp_prefix.sc2sr.primary.sv.bam \
	   OUTPUT_UNORDERED_RECORDS=$tmp_prefix.sc2sr.supp.sv.bam \
	   WORKER_THREADS=$threads \
	   $picardoptions \
      \&\& $rmcmd $tmp_prefix.coordinate.bam \
      \&\& $timecmd samtools sort \
		  -@ $threads \
		  -T $tmp_prefix.sc2sr.suppsorted.sv-tmp \
		  -Obam \
		  -o $tmp_prefix.sc2sr.suppsorted.sv.bam \
		  $tmp_prefix.sc2sr.supp.sv.bam \
      \&\& $rmcmd $tmp_prefix.sc2sr.supp.sv.bam \
      \&\& $timecmd samtools merge \
		  -@ $threads \
		  $prefix.sv.tmp.bam \
		  $tmp_prefix.sc2sr.primary.sv.bam \
		  $tmp_prefix.sc2sr.suppsorted.sv.bam \
      \&\& $timecmd samtools index $prefix.sv.tmp.bam \
      \&\& $rmcmd $tmp_prefix.sc2sr.primary.sv.bam \
      \&\& $rmcmd $tmp_prefix.sc2sr.suppsorted.sv.bam \
      \&\& mv $prefix.sv.tmp.bam $prefix.sv.bam \
      \&\& mv $prefix.sv.tmp.bam.bai $prefix.sv.bam.bai \
  \; \} 1\>\&2 2\>\> $logfile\'
eval "parse $softcliptstosplitreads"
eval "$(parse $softcliptstosplitreads)"

preprocessforbreakendassembly='{ $timecmd java -Xmx4g $jvm_args \
	   -cp $gridss_jar gridss.PreprocessForBreakendAssembly \
	   TMP_DIR=$dir \
	   WORKING_DIR=$workingdir \
	   REFERENCE_SEQUENCE=$reference \
	   COMPRESSION_LEVEL=0 \
	   I=$tmp_prefix.namedsorted.bam \
	   O=/dev/stdout \
	   WORKER_THREADS=$threads \
	   ALIGNER=BWAMEM \
	   ALIGNER_BATCH_SIZE=10000 \
	   $picardoptions \
      \| $timecmd samtools sort \
		 -@ $threads \
		 -T $tmp_prefix.sc2sr.suppsorted.sv-tmp \
		 -Obam \
		 -o $prefix.sv.tmp.bam \
		 /dev/stdin \
      \&\& $rmcmd $tmp_prefix.namedsorted.bam \
      \&\& $timecmd samtools index $prefix.sv.tmp.bam \
      \&\& mv $prefix.sv.tmp.bam $prefix.sv.bam \
      \&\& mv $prefix.sv.tmp.bam.bai $prefix.sv.bam.bai \
  \; \} 1\>\&2 2\>\> $logfile\'
eval "parse $preprocessforbreakendassembly"
eval "$(parse $preprocessforbreakendassembly)"


sanitycheckevidence='java -Xmx$jvmheap $jvm_args \
     -cp $gridss_jar gridss.SanityCheckEvidence \
     TMP_DIR=$workingdir \
     WORKING_DIR=$workingdir \
     REFERENCE_SEQUENCE=$reference \
     WORKER_THREADS=$threads \
     $input_args \
     $blacklist_arg \
     $config_args \
     ASSEMBLY=ignored \
     OUTPUT_ERROR_READ_NAMES=reads_failing_sanity_check.txt \
     1\>\&2 2\>\> $logfile\'
eval "parse $sanitycheckevidence"
eval "$(parse $preprocessforbreakendassembly)"

assemblebreakends='{ $timecmd java -Xmx$jvmheap $jvm_args \
	   -Dgridss.output_to_temp_file=true \
	   -cp $gridss_jar gridss.AssembleBreakends \
	   JOB_INDEX=$jobindex \
	   JOB_NODES=$jobnodes \
	   TMP_DIR=$workingdir \
	   WORKING_DIR=$workingdir \
	   REFERENCE_SEQUENCE=$reference \
	   WORKER_THREADS=$threads \
	   O=$assembly \
	   $input_args \
	   $blacklist_arg \
	   $config_args \
	   $picardoptions \
	   $readpairing_args \
  \; \} 1\>\&2 2\>\> $logfile\'
eval "parse $assemblebreakends"
eval "$(parse $assemblebreakends)"

collectgridssmetrics2='{ $timecmd java -Xmx4g $jvm_args \
	   -cp $gridss_jar gridss.analysis.CollectGridssMetrics \
	   I=$assembly \
	   O=$prefix \
	   REFERENCE_SEQUENCE=$reference \
	   THRESHOLD_COVERAGE=$maxcoverage \
	   TMP_DIR=$workingdir \
	   FILE_EXTENSION=null \
	   GRIDSS_PROGRAM=null \
	   GRIDSS_PROGRAM=CollectCigarMetrics \
	   GRIDSS_PROGRAM=CollectMapqMetrics \
	   GRIDSS_PROGRAM=CollectTagMetrics \
	   GRIDSS_PROGRAM=CollectIdsvMetrics \
	   GRIDSS_PROGRAM=ReportThresholdCoverage \
	   PROGRAM=null \
	   PROGRAM=QualityScoreDistribution \
	   $picardoptions \
  \; \} 1\>\&2 2\>\> $logfile\'
eval "parse $collectgridssmetrics2"
eval "$(parse $collectgridssmetrics2)"

softcliptstosplitreads2='{ $timecmd java -Xmx4g $jvm_args \
	   -Dgridss.async.buffersize=16 \
	   -Dsamjdk.create_index=false \
	   -Dgridss.output_to_temp_file=true \
	   -cp $gridss_jar gridss.SoftClipsToSplitReads \
	   TMP_DIR=$dir \
	   WORKING_DIR=$workingdir \
	   REFERENCE_SEQUENCE=$reference \
	   WORKER_THREADS=$threads \
	   I=$assembly \
	   O=$tmp_prefix.sc2sr.primary.sv.bam \
	   OUTPUT_UNORDERED_RECORDS=$tmp_prefix.sc2sr.supp.sv.bam \
	   REALIGN_ENTIRE_READ=true \
	   READJUST_PRIMARY_ALIGNMENT_POSITION=true \
	   $aligner_args_bwa \
	   $picardoptions \
      \&\& $timecmd samtools sort \
		  -@ $threads \
		  -T $tmp_prefix.sc2sr.suppsorted.sv-tmp \
		  -Obam \
		  -o $tmp_prefix.sc2sr.suppsorted.sv.bam \
		  $tmp_prefix.sc2sr.supp.sv.bam \
      \&\& $rmcmd $tmp_prefix.sc2sr.supp.sv.bam \
      \&\& $timecmd samtools merge \
		  -@ $threads \
		  $prefix.sv.tmp.bam \
		  $tmp_prefix.sc2sr.primary.sv.bam \
		  $tmp_prefix.sc2sr.suppsorted.sv.bam \
      \&\& $timecmd samtools index $prefix.sv.tmp.bam \
      \&\& $rmcmd $tmp_prefix.sc2sr.primary.sv.bam \
      \&\& $rmcmd $tmp_prefix.sc2sr.suppsorted.sv.bam \
      \&\& mv $prefix.sv.tmp.bam $prefix.sv.bam \
      \&\& mv $prefix.sv.tmp.bam.bai $prefix.sv.bam.bai \
  \; \} 1\>\&2 2\>\> $logfile\'
eval "parse $softcliptstosplitreads2"
eval "$(parse $softcliptstosplitreads2)"

softcliptstosplitreads2_int='{ $timecmd java -Xmx4g $jvm_args \
	   -Dgridss.async.buffersize=16 \
	   -Dsamjdk.create_index=false \
	   -cp $gridss_jar gridss.SoftClipsToSplitReads \
	   TMP_DIR=$dir \
	   WORKING_DIR=$workingdir \
	   REFERENCE_SEQUENCE=$reference \
	   WORKER_THREADS=$threads \
	   I=$assembly \
	   O=/dev/stdout \
	   ALIGNER=BWAMEM \
	   ALIGNER_BATCH_SIZE=100000 \
	   REALIGN_ENTIRE_READ=true \
	   READJUST_PRIMARY_ALIGNMENT_POSITION=true \
	   COMPRESSION_LEVEL=0 \
	   $picardoptions \
      \| $timecmd samtools sort \
		 -@ $threads \
		 -T $tmp_prefix.sc2sr.suppsorted.sv-tmp \
		 -Obam \
		 -o $prefix.sv.tmp.bam \
		 /dev/stdin \
      \&\& $timecmd samtools index $prefix.sv.tmp.bam \
      \&\& mv $prefix.sv.tmp.bam $prefix.sv.bam \
      \&\& mv $prefix.sv.tmp.bam.bai $prefix.sv.bam.bai \
  \; \} 1\>\&2 2\>\> $logfile\'
eval "parse $softcliptstosplitreads2_int"
eval "$(parse $softcliptstosplitreads2_int)"

sanitycheckbyevidence2='java -Xmx$jvmheap $jvm_args \
     -cp $gridss_jar gridss.SanityCheckEvidence \
     TMP_DIR=$workingdir \
     WORKING_DIR=$workingdir \
     REFERENCE_SEQUENCE=$reference \
     WORKER_THREADS=$threads \
     $input_args \
     $blacklist_arg \
     $config_args \
     ASSEMBLY=$assembly \
     $readpairing_args \
     OUTPUT_ERROR_READ_NAMES=reads_failing_sanity_check.txt\'
eval "parse $sanitycheckbyevidence2"
eval "$(parse $sanitycheckbyevidence2)"
eval "echo $sanitycheckbyevidence2"
eval eval "$(echo $sanitycheckbyevidence2)"

identifyvariants='$timecmd java -Xmx$jvmheap $jvm_args \
	 -Dgridss.output_to_temp_file=true \
	 -cp $gridss_jar gridss.IdentifyVariants \
	 TMP_DIR=$workingdir \
	 WORKING_DIR=$workingdir \
	 REFERENCE_SEQUENCE=$reference \
	 WORKER_THREADS=$threads \
	 $input_args \
	 $blacklist_arg \
	 $config_args \
	 ASSEMBLY=$assembly \
	 OUTPUT_VCF=$prefix.unallocated.vcf \
	 $readpairing_args'
eval "echo $identifyvariants"
eval eval "$(echo $ok)"


##########
##########
########## Look here!
##########
##########
blip="fii"
ok='{ echo \ "$blip foob" \| sed "s/i/o/g"\; echo "finish"\; \}'

parse() {
    echo "$@" | xargs | xargs
}
# eval parse "$ok"
# eval "$(parse \"$ok\")"
# eval $(parse "$ok")
eval "parse $ok"
eval "$(parse $ok)"

eval "echo $ok"
eval eval "$(echo $ok)"

eval echo "$ok" # this is it
eval eval "$ok" # this is it
##########
##########
########## Look here!
##########
##########

    # echo "Matching provided reference against known library of blacklists"


    # ## using reference blacklist lookup to match up reference
    # ent=$( cat ${RB_LOOKUP} | \
    #	       awk -v var=$(readlink -f $REFERENCE) '$1 == var' | xargs)

    # matchbl=$(echo "$ent" | awk '{print $2}' | xargs -I {} readlink -f {})
    # matchpon=$(echo "$ent" | awk '{print $3}' | xargs -I {} readlink -f {})

    # ## default blacklist
    # # "/gpfs/commons/groups/imielinski_lab/DB/GRIDSS/core.svaba.blacklist.bed"
    # if [ "$ent" ]; then

    #	echo "Reference match found"
    #	test "$matchbl" == "$BLACKLIST" && echo "provided blacklist good to go"
    #	if [ ! "$BLACKLIST" == "$matchbl" ]; then
    #	    echo "Blacklist provided (path: $BLACKLIST) does not match known working file"
    #	    echo "replacing with good match: ${matchbl}"
    #	    BLACKLIST=${matchbl}
    #	fi
    #	test "$matchpon" == "$PONDIR" && echo "provided PON directory good to go"
    #	if [ ! "$PONDIR" == "$matchpon" ]; then
    #	    echo "PON provided (path: $PONDIR) does not match known working file"
    #	    echo "replacing with good match: ${matchpon}"
    #	    PONDIR=${matchpon}
    #	fi
    # else
    #	{ test ! -e "$BLACKLIST" || test "$BLACKLIST" == "/dev/null"; } &&
    #	    echo "Blacklist file ($BLACKLIST) not valid" &&
    #	    BLACKLIST="${LIBDIR}/core.svaba.blacklist.bed" &&
    #	    echo "Replacing with default, $BLACKLIST"
    #	echo "Reference not found in reference fasta/blacklist dictionary..."
    #	echo -e \
    #	     "\nWARNING: GRIDSS requires the blacklist must not contain non-matching seqnames to ref"

    #	{ test ! -e "$PONDIR" || test "$PONDIR" == "/dev/null"; } &&
    #	    echo "PONDIR file ($PONDIR) not valid" &&
    #	    PONDIR="" &&
    #	    echo "Not using PON!"
    #	echo "PON directory (${PONDIR}) not found in reference fasta/PON dictionary..."
    #	echo -e \
    #	     "\nWARNING: GRIDSS requires the pon must not contain non-matching seqnames to ref, i.e. chr vs no chr"
    # fi

    # cat ${REFERENCE}.fai | awk '{print $1}' | uniq > rname
    # cat ${BLACKLIST} | awk '{print $1}' | uniq > bname


    # allbninrn=$(Rscript -e "cat(tolower(all(readLines('bname') %in% readLines('rname'))))")
    # bnnotinrn=$(Rscript -e "cat({bn = readLines('bname'); bn[!bn %in% readLines('rname')]})")

    # $allbninrn && echo "Passed: all seqnames in ${BLACKLIST} are in ${REFERENCE}.fai" ||
    #	    { echo "ERROR!! Found $blacklist contains seqnames not within ${REFERENCE}.fai" &&
    #		  echo "GRIDSS will break... rewriting blacklist without offending seqnames to file:" &&
    #		  echo "fixed_blacklist.bed"
    #	      awk -v var="$bnnotinrn" 'BEGIN{split(var,t); for (i in t) vals[t[i]]} !($1 in vals)' \
    #		  $BLACKLIST > fixed_blacklist.bed &&
    #		  BLACKLIST="fixed_blacklist.bed" &&
    #		  echo "now using ./fixed_blacklist.bed" \
    #		      ; }


WDIR2=""
if [ -s "wdir" ]; then
    WDIR2=$(cat wdir)
fi    

if [ ! "$WDIR" = "." ]; then
    if [ ! -s "$WDIR2" ]; then
	test ! -s "$WDIR" && test -s "/scratch" && WDIR="/scratch" ||
		WDIR="."
	WDIR="$WDIR/$(basename $(readlink -f .))_$$"
	echo $WDIR > wdir
    else
	WDIR=$WDIR2
    fi
fi
mkdir -p $WDIR


#' khadi Tuesday, Aug 25, 2020, Week 35, 04:03:49 PM

(module load speedseq; export PYTHONPATH=""; module unload python; module load python/2.7.8; python $(dirname $(which speedseq))/bamtofastq.py)

(module load speedseq; export PYTHONPATH=""; module unload python; module load python/2.7.8; python $(dirname $(which speedseq))/bamtofastq.py)


## this works for realignment
( module load speedseq; export PYTHONPATH=""; module unload python; module load python/2.7.8;  samtools view -F 2304 /gpfs/commons/groups/imielinski_lab/data/dbGAP/dbGaP-9764/sra/SRR3882763.sra.bam | python $(dirname $(which speedseq))/bamtofastq.py | $(dirname $(which speedseq))/mbuffer -q -m 1G )


set -o pipefail
if { samtools view mini_R1.fastq.gz > /dev/null | head; }; then echo "fi"; fi


cat acralmelanoma.fasta | sed 's/^>\([0-9]\|\(X\|Y\|MT\)\+\)/\>chr\1/' | grep "^>"




star_cmd=$(get_fn ~/modules/STAR_Fusion/STAR); mkdir -p ~/DB/STAR/hg19/v_2.7.0f/overhang82; $star_cmd --runMode genomeGenerate --genomeDir ~/DB/STAR/hg19/v_2.7.0f/overhang82 --genomeFastaFiles ~/DB/GATK/chr_acralmelanoma.fasta --sjdbGTFfile ~/DB/GENCODE/gencode.v19.annotation.gtf --sjdbOverhang 82 --runThreadN 8



libdir=/gpfs/commons/home/khadi/modules/STAR_Fusion/
genome_lib_dir=/gpfs/commons/groups/imielinski_lab/DB/modules/STAR_Fusion/star_fusion_dir/GRCh37_gencode_v19_CTAT_lib_Apr032020.plug-n-play/ctat_genome_lib_build_dir
left_fq=/gpfs/commons/groups/imielinski_lab/projects/Tyfonas/Flow/bamToFastq/0934/converted_end1.fq
right_fq=/gpfs/commons/groups/imielinski_lab/projects/Tyfonas/Flow/bamToFastq/0934/converted_end2.fq
filter_fusions=TRUE


singularity exec -B $(dirname ${left_fq}):/lfq -B $(dirname ${right_fq}):/rfq -B $(readlink -f $(pwd)):/outpath -B ${genome_lib_dir}:${genome_lib_dir} ${libdir}/star-fusion.v1.9.0.simg /usr/local/src/STAR-Fusion/STAR-Fusion --left_fq /lfq/$(basename ${left_fq}) --right_fq /rfq/$(basename ${right_fq}) --CPU 8 --genome_lib_dir ${genome_lib_dir} ${filter_arg} -O /outpath --FusionInspector validate --examine_coding_effect --denovo_reconstruct


(
    module unload anaconda3
    module load anaconda3/10.19
    export PYTHONPATH=/nfs/sw/anaconda3/anaconda3-10.19/lib/python3.7:$PYTHONPATH
    conda activate pizzly
)
 
(
    . anaconda3
    conda activate pizzly
)


#' khadi Friday, Sep 11, 2020, Week 37, 08:33:12 PM

echo "$@" | sed "s/\(--fusions\) \(FALSE\|TRUE\)/\1 TRUE/"


cmd="singularity run \
       -B ${refdir}:/refdata \
       -B "${out_path}":/data \
       -B ${tbam_dir}:${tbam_dir} \
       -B ${nbam_dir}:${nbam_dir} \
       -B /gpfs/commons/groups/imielinski_lab/DB/GATK:/gpfs/commons/groups/imielinski_lab/DB/GATK \
       -B ${out_path}/input_links:/inputs \
       ${libdir}/gpl_sandbox/gpl.sif \
       -n /inputs/normal.bam \
       -t /inputs/tumor.bam \
       -v /data/gridss-purple-linx-latest.vcf.gz \
       -s ${id} \
       --snvvcf /inputs/${id}_strelka.vcf.gz \
       --jvmheap ${mem}g \
       --threads ${cores} \
       --reference ${refpath}"

echo "$(echo ${cmd})" # to echo $cmd without any of the white space




export CMD2=$(echo $CMD | sed 's/\\[[:space:]]\+/ /g' | sed 's/[[:space:]]\{2,\}/ /g')



export CMD='singularity exec \
		-B ${libdir}:/libdir \
		-B ${tbam_dir}:${tbam_dir} \
		-B ${nbam_dir}:${nbam_dir} \
		-B ./input_links:/inputs \
		-B "${out_path}":/data \
		-B ${refdir}:/refdata \
		-B ${TMP}:${TMP} \
		-B ${TMPDIR}:${TMPDIR} \
		-B ${refmeta}:/refmeta ${libdir}/cgpindel_fix.sif \
		sh -c '"'"'{ perl /opt/wtsi-cgp/bin/pindel.pl -tumour /inputs/tumor.bam \
		-normal /inputs/normal.bam \
		-reference /refdata/${refbasepath} \
		-exclude NC_007605,hs37d5,GL% \
		-simrep /refmeta/simpleRepeats.bed.gz \
		-badloci /refmeta/hiSeqDepth.bed.gz \
		-genes /refmeta/codingexon_regions.indel.bed.gz \
		-unmatched /refmeta/pindel_np.gff3.gz \
		-assembly GRCh37d5 \
		-species Human \
		-seqtype WGS \
		-filter /refmeta/genomicRules.lst \
		-softfil /refmeta/softRules.lst \
		-outdir /data/result \
		-debug \
		-cpus ${cores}; echo $? > /data/exit_status; } 2>&1 | tee /data/run.log'"'"''

export CMD2=$(echo $CMD | sed 's/\\[[:space:]]\+/ /g' | sed 's/[[:space:]]\{2,\}/ /g')

{ echo "Running" && echo "$(echo ${CMD2})" && eval ${CMD2}; }

export ex=$(cat exit_status)

if [ ! $ex = 0 ]; then
    echo "cgpPindel broke"
    exit $ex
fi

isme="TRUE"
len=255

while (( "$#" )); do
    case "$1" in
	-a|--all)
	    isme="FALSE"
	    shift
	    ;;
	-l|--length)
	    len=$1
	    shift
	    ;;
	--)
	    shift
	    break
	    ;;
	-*|--*=)
	echo "Error: Unsupported flag $1" >&2
	exit 1
	;;
	*)
	    break
	    ;;
    esac
done



    




























# script for slurm sstat
#!/bin/bash

isme="TRUE"
len=255

while (( "$#" )); do
    case "$1" in
	-a|--all)
	    isme="FALSE"
	    shift
	    ;;
	-l|--length)
	    len=$1
	    shift
	    ;;
	--)
	    shift
	    break
	    ;;
	-*|--*=)
	echo "Error: Unsupported flag $1" >&2
	exit 1
	;;
	*)
	    break
	    ;;
    esac
done


var="jobid,state,name,username,groupname,reason,timelimit,timeused,submittime,starttime,minmemory,numcpus,numnodes,priority,nice"

IFS=","
thisvar=""

while read i; do
    thisvar=$(echo $i)
done <<< ${var}

IFS=" "
newvar=""
for myval in ${thisvar}
do
    # newvar=$(echo ${newvar}${myval}:.${len},) # . = right justified
    newvar=$(echo ${newvar}${myval}:${len},)
done


if [ "$isme" = "TRUE" ]
then
    CMD="squeue -O ${newvar} | sed 's/[[:space:]]\{2,\}/\t/g' | awk -v var=\$(whoami) 'NR==1 {print}; \$4 == var {print \$0}'"
elif [ "$isme" = "FALSE" ]
then
    CMD="squeue -O ${newvar} | sed 's/[[:space:]]\{2,\}/\t/g'"
fi

# echo $CMD

eval $CMD


























vcftools --gzvcf /gpfs/commons/groups/imielinski_lab/DB/Pubs/PCAWG/marker/snv_indel/DO52732_fixed2.vcf.gz --remove-indels --remove-filtered-all --recode --stdout | awk '
{
    if (match($0,/(##contig=<ID=)(.*)/,m)) {
         sub(/chr/,"", m[2]); print m[1]m[2];
    } else if ($0 !~ /^#/) {
         gsub(/^chr/,""); print;
    } else {
         print $0 
    }
}' | bedtools intersect -a stdin -b /gpfs/commons/groups/imielinski_lab/projects/SV_Signatures/Flow/hrdetect/20220207/hrdetect_nonbopp/DO52732/good_rfile.bed -header | vcf-sort -c | bcftools view -S ^./excls.txt | bcftools view -v snps | bcftools norm -Ov -m-any;
