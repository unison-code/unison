UNIARGS = --lintpragma=false

.PRECIOUS: %.uni %.lssa.uni %.ext.uni %.alt.uni %.json

factorial-examples: factorial.mir
	rm -f *.uni *.json
	make factorial.json

%.alt.uni: %.ext.uni
	uni augment $(UNIARGS) $< -o $@

%.ext.uni: %.lssa.uni
	uni extend $(UNIARGS) $< -o $@

%.lssa.uni: %.uni
	uni linearize $(UNIARGS) $< -o $@

%.uni: %.mir
	uni import $(UNIARGS) --nocc --noreserved --goal=speed $< -o $@

%.json : %.alt.uni %.llvm.mir
	uni model --basefile=$*.llvm.mir $< | aeson-pretty -c > $@

%.llvm.mir : %.asm.mir
	uni normalize $< -o $@
