my @out=( 
[1,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0],
[0,0,1,0,0,0,0,0],
[0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0]);

$i=0;
while(<STDIN>) 
{
    if(!/^[#].*/) {
        chop();
        chop();
#        print "reading $_\n";
        $_=~tr/ \*/01/;
        $byte[$i++]=$_;
    } else {
#        print "processing $_\n";
        $comment=$_;
        chop($comment);
        $type=3;
        $i=0;
        while($i<8) 
        {
            $foo = length($byte[$i]);
            $foo  == 8 || die("not 8 bites, only $foo :$byte[$i]\n");
            for($j=0;$j<8;$j++) {
                $bit=substr($byte[$i],$j,1);
                if($type ==0) {
                    $out[7-$j][7-$i]=$bit;
                } elsif($type ==1) {
                    $out[7-$i][7-$j]=$bit; 
                } elsif($type ==2) {
                    $out[$j][7-$i]=$bit; 
                } elsif($type ==3) {
                    $out[$i][$j]=$bit; 
                } else {
                    die("unknown rotate type\n");
                }
            }
            $i++;
        }
        $label=substr($comment,2);
        print "$label\n";
        for($i=0;$i<8;$i++) {
            print "    ds 1,%";
            for($j=0;$j<8;$j++) {
                print "$out[$i][$j]";
            }
            print "\n";
        }
        $i=0;
    }
}


