#!/usr/bin/perl -w

$title = "The Burro Programming Language, v2.0";
$lhs_input = "../src/Burro.lhs";
$lhs_temp = "lhs.tmp";
$html_temp = "html.tmp";
$html_output = "burro.html";

open INFILE, "<$lhs_input";
open OUTFILE, ">$lhs_temp";

while (defined ($line = <INFILE>)) {
  chomp $line;
  if ($line =~ /^\>(.*?)coding\:(.*?)$/) {
    # pass
  } elsif ($line =~ /^\>(.*?)$/) {
    print OUTFILE "    $1\n";
  } else {
    $line =~ s/\s\-\-\s/ \&mdash\; /g;
    print OUTFILE "$line\n";
  }
}

close INFILE;
close OUTFILE;

system "Markdown.pl <$lhs_temp >$html_temp";

open INFILE, "<$html_temp";
open OUTFILE, ">$html_output";

print OUTFILE <<"EOH";
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en">
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
<title>$title</title>
<link rel="stylesheet" type="text/css" href="markdown-lhs.css" />
</head>
<body>
EOH

while (defined ($line = <INFILE>)) {
    print OUTFILE $line;
}

print OUTFILE <<"EOF";
</body>
</html>
EOF

close INFILE;
close OUTFILE;

system "rm *.tmp";
