ifeq (, $(shell which vasm))
 AS = vasmm68k_mot
 #$(info "vasm not found")
else
 AS = vasm
 #$(info  "vasm found")
endif


star4kd: star4kd.s 1.raw
	$(AS) -nosym -m68020 -Fhunkexe star4kd.s -o star4kd
	ls -l star4kd


clean:
	rm -f star4kd

