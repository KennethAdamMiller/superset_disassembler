source tag.sh
cat ../configs/broker.yaml | envsubst | kubectl apply
cat ../configs/analysis.yaml | envsubst | kubectl apply
