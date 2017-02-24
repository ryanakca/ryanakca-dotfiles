aklogs () {
    for r in CS.CMU.EDU ANDREW.CMU.EDU; do
        KRB5CCNAME=`klist -l | grep $r | sed -e s'/.*:://g'` aklog -c $r -k $r
    done
}
