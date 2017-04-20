WARDEN=${1:-./dist/build/warden/warden}
WARDEN_GEN=${2:-./dist/build/warden-gen/warden-gen}
WARDEN_SAMPLE=${3:-./dist/build/warden-sample/warden-sample}
export AWS_DEFAULT_REGION="ap-southeast-2"
export AMBIATA_ENV="test"

banner () {
    echo
    echo ===========
    echo == "$*"
    echo ===========
    echo
}

fail () {
    echo "  \`- [FAILED] $1"
    exit 1
}

check () {
    [ "$1" = "$2" ] || ( echo "FAIL: $1 != $2" ; exit 1 )
}
