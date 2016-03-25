#!/bin/sh

echo "Cleaning up..."
rm 00-index.tar.gz
mkdir -p package
echo "Downloading index..."

if [ ! -e 00-index.tar.gz ] ; then 
  wget http://hackage.haskell.org/packages/archive/00-index.tar.gz
fi

for splitpk in `tar tf 00-index.tar.gz | grep -v preferred | cut -d/ -f 1,2`; do
	pk=`echo $splitpk | sed 's|/|-|'`
        echo Downloading $pk ...
	name=$pk.tar.gz
	if [ ! -e package/$name ]; then
	   wget http://hackage.haskell.org/package/$pk/$name -O package/$name
	fi
done

# https://hackage.haskell.org/package/lvish-1.1.4/lvish-1.1.4.tar.gz
