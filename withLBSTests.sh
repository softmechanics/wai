#!/bin/bash

cd $(dirname $0)

curl -vv --data-binary @Network/Wai/Util.hs http://localhost:3000/postWithLBSException/
curl -vv --data-binary @Network/Wai/Util.hs http://localhost:3000/postWithLBSComplete/
curl -vv --data-binary @Network/Wai/Util.hs http://localhost:3000/postWithLBSPartial/

