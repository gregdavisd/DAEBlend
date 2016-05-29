# Blender-Collada
Python script to export Collada DAE files from Blender.

There is no stable release yet so clone the latest files from the repository to get the latest bug fixes and features.

Currently exports:
- Meshes and curves, including meshes generated from a curve.
- Animation actions or the complete animation timeline
- Rigid body collision shapes
- Materials and textures
- Armatures and skin deformations, bone parenting.
- Morph targets (shape keys)
- Cameras
- Lamps

Elements are given meaningful identifiers that can be related back to the original Blender document. Relating the XML tags back to Blender helps when editing the XML document post export.

Originally based on the 'Better Collada' exporter the code has been refactored and features added. The generated output has differences in nuance from the output of the original script. So this exporter isn't always drop-in compatible with 'Better Collada'.

WARNING: DO NOT RE-IMPORT INTO BLENDER
Reimporting textured scenes into Blender using the builtin importer will cause Blender to crash immediately. This is due to Blender's builtin Collada expecting dae files to use nonsensical image handling tags. For instance Blender Collada uses a 2D Nurbs <surface> tag to hold image references in the effect library. Using correct image references causes Blender to crash possibly causing data loss.

Send bugs/queries requests to gregery20@yahoo.com.au or raise an issue on github.
