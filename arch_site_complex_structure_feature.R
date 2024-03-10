library(data.tree)

ArchComplex <- Node$new("Site")
complex <- ArchComplex$AddChild("Complex_A")
structure <- complex$AddChild("Structure_1")
feature <- structure$AddChild("Feature_A")
feature <- structure$AddChild("Feature_B")
feature <- structure$AddChild("Feature_C")
structure <- complex$AddChild("Structure_2")
feature <- structure$AddChild("Feature")
complex <- ArchComplex$AddChild("Complex_B")
structure <- complex$AddChild("Structure_1")
feature <- structure$AddChild("Feature_A")
feature <- structure$AddChild("Feature_B")
feature <- structure$AddChild("Feature_C")
structure <- complex$AddChild("Structure_2")
feature <- structure$AddChild("Feature")

SetGraphStyle(, rankdir = "TB")
SetEdgeStyle(ArchComplex, arrowhead = "vee", color = "grey35", penwidth = 2)
SetNodeStyle(ArchComplex, style = "filled,rounded", shape = "box", fillcolor = "LightBlue",
             fontname = "helvetica", tooltip = GetDefaultTooltip)
ArchComplex$

plot(ArchComplex)

