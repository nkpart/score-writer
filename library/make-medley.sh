set -e

score-writer render --landscape --score-file lady-mackenzie-of-fairbairn.score --output-file lady-mackenzie.pdf

/usr/local/bin/gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=medley.pdf lady-mackenzie.pdf
