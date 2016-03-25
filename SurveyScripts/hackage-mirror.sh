#!/bin/sh

outdir=./data/0_hackage_all_tarballs/

echo "Cleaning up..."
rm 00-index.tar.gz
mkdir -p $outdir
echo "Downloading index..."

if [ ! -e 00-index.tar.gz ] ; then
  wget http://hackage.haskell.org/packages/archive/00-index.tar.gz
fi

for splitpk in `tar tf 00-index.tar.gz | grep -v preferred | cut -d/ -f 1,2`; do
	pk=`echo $splitpk | sed 's|/|-|'`
        echo Downloading $pk ...
	name=$pk.tar.gz
	if [ ! -e package/$name ]; then
	   wget http://hackage.haskell.org/package/$pk/$name -O $outdir/$name
	fi
done

# Example package URL:
# https://hackage.haskell.org/package/lvish-1.1.4/lvish-1.1.4.tar.gz
