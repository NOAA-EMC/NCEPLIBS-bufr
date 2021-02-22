#!/bin/bash

set -eu

cmd=./${1}

exit `${cmd} | grep -ic FAILED`
