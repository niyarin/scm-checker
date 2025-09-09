VERSION=$1
cd /
echo $VERSION
if [ "$VERSION" = "latest" ]; then
    echo "VERISION IS LATEST!"
    git clone http://github.com/niyarin/scm-checker
else
    git clone http://github.com/niyarin/scm-checker -b $VERSION
fi

cd /scm-checker
git submodule update --init

/cross-chicken/bin/i686-csc -X r7rs -R r7rs -static /scm-checker/src/main.scm -o "/generated-bin/scm-checker-${VERSION}-linux-i686"
/cross-chicken/bin/aarch64-csc -X r7rs -R r7rs -static /scm-checker/src/main.scm -o "/generated-bin/scm-checker-${VERSION}-linux-aarch64"
csc -X r7rs -R r7rs -static /scm-checker/src/main.scm -o "/generated-bin/scm-checker-${VERSION}-linux-x86_64"
