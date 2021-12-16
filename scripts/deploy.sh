source scripts/tag.sh
source scripts/feature_suffix.sh
cat ../configs/analysis.yaml | envsubst | kubectl apply
cat ../configs/broker.yaml | envsubst | kubectl apply
