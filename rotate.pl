my @out=( 
['1',0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0],
[0,0,1,0,0,0,0,0],
[0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0]);

$i=0;
while(<STDIN>) {
    chop();
    if($_ != "#") {
        $byte[$i++]=$_;
    } else {
        $comment=$_;
        $i=0;
        while($i<8) 
        {
            chop($byte[$i]);
            $foo = length($byte[$i]);
            $foo  == 8 || die("not 8 bites, only $foo\n");
            for($j=0;$j<8;$j++) {
                $bit=substr($byte[$i],$j,1);
#        print "bit=$bit\n";
#up
#        $out[7-$j][7-$i]=$bit;
# flip left
#        $out[7-$i][7-$j]=$bit; 
#down
                $out[$j][7-$i]=$bit; 
            }
            $i++;
        }
        print ";;; shape $comment\n";
        for($i=0;$i<8;$i++) {
            for($j=0;$j<8;$j++) {
                print "$out[$i][$j] ";
            }
            print "\n";
        }

}


