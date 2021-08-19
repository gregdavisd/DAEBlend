# Blender-Collada
Python script to export Collada Version 1.5 DAE files from Blender. Blender already has a builtin Collada exporter
but it's not an easily modifiable Python script addon.

Download link: 

https://minhaskamal.github.io/DownGit/#/home?url=https://github.com/gregdavisd/DAEBlend/tree/master/io_scene_dae

Extract the 'io_scene_dae' folder from the zip into the Blender addons folder:
\Program Files\Blender Foundation\Blender N.nn\N.nn\scripts\addons

(N.nn) is the version of Blender installed.

Open Blender Preferences -> Addons. Search for the Export addon 'Import-Export: Python Collada Exporter'.
Click the check box to enable it. If it was successfully installed then under File->Export the
'Collade (Python Script) option will now be available.

Customise by modifying the script and contribute changes to this repo.

Currently exports:
- Meshes and curves, including meshes generated from a curve.
- Animation actions from the Non Linear Editor (NLA) or the animation timeline.
- Armatures and skin deformations.
- Morph animations with shape key values.
- Rigid animation by parenting meshes to armature bones. Armatures can also be parented to a bone of another armature allowing for chains of armatures.
- Different coordinate systems for OpenGL and DirectX. Changes geometry data so doesn't use tricks like adding rotation nodes.
- Cameras.
- Lights.
- Rigid body collision shapes and physics information from within Blender.

Send bugs/queries requests to gregery20@yahoo.com.au or raise an issue on github.
