BUILD_DIR = build
DIR = java

jcompile:
$(BUILD_DIR)/$(JDIR)/%.class: $(JDIR)/%.java
	@ mkdir -p $(BUILD_JDIR)/$(JDIR)
	@ javac -cp $(JDIR) -d $(BUILD_JDIR)/$(JDIR) $(JAVA_OPTIONS) -implicit:none $<
	@ printf "%8s %-60s %s\n" javac $< "$(JAVA_OPTIONS)"