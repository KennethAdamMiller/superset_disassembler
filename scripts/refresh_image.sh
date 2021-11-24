source scripts/tag.sh
./dockerFlambda.sh
docker tag superset_disasm_flambda:${TAG} ${registry}superset_disasm_flambda:${TAG} 
docker push ${registry}superset_disasm_flambda:${TAG}
