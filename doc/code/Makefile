UNIARGS = --lintpragma=false

.PRECIOUS: %.uni %.lssa.uni %.ext.uni %.alt.uni

factorial-examples: factorial.mir
	rm -f *.uni
	make factorial.alt.uni

%.alt.uni: %.ext.uni
	uni augment $(UNIARGS) $< -o $@

%.ext.uni: %.lssa.uni
	uni extend $(UNIARGS) $< -o $@

%.lssa.uni: %.uni
	uni linearize $(UNIARGS) $< -o $@

%.uni: %.mir
	uni import $(UNIARGS) --nocc --noreserved $< -o $@
