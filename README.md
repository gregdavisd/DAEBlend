# Blender-Collada
Python script to export Collada Version 1.5 DAE files from Blender.

Currently exports:
- Meshes and curves, including meshes generated from a curve.
- Animation actions from the Non Linear Editor (NLA) or the animation timeline.
- Rigid body collision shapes and physics information from within Blender mode or Blender Game Engine mode.
- Materials and textures from Blender Render.
- Armatures and skin deformations.
- Rigid animation by parenting meshes to armature bones. Armatures can also be parented to a bone of another armature allowing for chains of armatures.
- Morph targets (shape keys). Morph Key animation is experimental as the official Collada spec doesn't support it.
- Cameras.
- Lamps.

Elements are given meaningful identifiers that can be related back to the original Blender document. Relating the XML tags back to Blender helps when working with the XML document post export.

Originally based on the 'Better Collada' exporter the code has been refactored and features added. The generated output has differences in nuance from the output of the original script. So this exporter isn't always drop-in compatible with 'Better Collada'.

Send bugs/queries requests to gregery20@yahoo.com.au or raise an issue on github.
