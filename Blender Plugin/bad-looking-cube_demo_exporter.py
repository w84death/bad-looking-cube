import bpy
import csv
import os
import math
import re

bl_info = {
    "name": "Bad Looking Cube B3D Collection Exporter",
    "author": "Krzysztof Krystian Jankowski",
    "version": (1, 0),
    "blender": (2, 80, 0),
    "location": "View3D > Sidebar > Bad Looking Cube Tab",
    "description": "Exports mesh data to a CSV file",
    "warning": "",
    "wiki_url": "",
    "category": "Import-Export",
}



class MeshExporterProperties(bpy.types.PropertyGroup):
    file_path: bpy.props.StringProperty(
        name="File Path",
        description="Path to save the CSV file",
        default="//screenplay.csv",
        subtype='FILE_PATH'
    )
    collection: bpy.props.StringProperty(
        name="Collection",
        description="Choose the collection for export"
    )

class MeshExporterPanel(bpy.types.Panel):
    bl_label = "Bad Looking Cube"
    bl_idname = "PT_BadLookingCube"
    bl_space_type = 'VIEW_3D'
    bl_region_type = 'UI'
    bl_category = 'Bad Looking Cube'

    def draw(self, context):
        layout = self.layout
        scene = context.scene
        mesh_exporter = scene.mesh_exporter
        layout.prop(mesh_exporter, "file_path")
        layout.prop_search(mesh_exporter, "collection", bpy.data, "collections")
        layout.operator("export.mesh_data")

class ExportMeshData(bpy.types.Operator):
    bl_idname = "export.mesh_data"
    bl_label = "Export Mesh Data"
    
    def clean_name(self, name):
        return re.sub(r"\.\d{3}$", "", name)
    
    def execute(self, context):
        mesh_exporter = context.scene.mesh_exporter
        csv_file_path = bpy.path.abspath(mesh_exporter.file_path)
        collection_name = mesh_exporter.collection
        if not collection_name:
            self.report({'ERROR'}, "No collection selected for export.")
            return {'CANCELLED'}
         
        collection = bpy.data.collections.get(collection_name)
        if not collection:
            self.report({'ERROR'}, f"Collection '{collection_name}' not found.")
            return {'CANCELLED'}

        if collection:
            # Open the CSV file for writing
            with open(csv_file_path, mode='w', newline='') as file:
                writer = csv.writer(file)
                
                for obj in collection.all_objects:
                   
                    if obj.type == 'MESH':
                        loc = obj.location
                        rot = obj.rotation_euler
                        rot_deg = [math.degrees(angle) % 360 for angle in rot]
                        scale = obj.scale.x
                        
                        cleaned_name = self.clean_name(obj.name)
                        
                        # Write the object's data to the CSV file
                        writer.writerow(['-1', 'clone', cleaned_name, -loc.x, loc.z, loc.y, rot_deg[0], rot_deg[2], rot_deg[1], scale])
        
                        if obj.animation_data and obj.animation_data.action:
                            # Extract keyframes for this object
                            keyframes = set()
                            for fcurve in obj.animation_data.action.fcurves:
                                for keyframe in fcurve.keyframe_points:
                                    keyframes.add(keyframe.co.x)  # co.x is the frame number

                            for frame in sorted(keyframes):
                                bpy.context.scene.frame_set(int(frame))  # Set the scene to the frame

                                # Convert frame to time
                                time = frame / bpy.context.scene.render.fps

                                # Get object's location and rotation in Euler angles
                                loc = obj.location
                                rot = obj.rotation_euler  # Assuming the object uses Euler rotation
                                rot_deg = [math.degrees(angle) % 360 for angle in rot]
                                
                                writer.writerow([time, 'pos', cleaned_name, -loc.x, loc.y, loc.z, rot_deg[0], rot_deg[2], rot_deg[1], scale])

        
        self.report({'INFO'}, f"Data exported to {csv_file_path}")
        return {'FINISHED'}
        

def register():
    bpy.utils.register_class(MeshExporterProperties)
    bpy.types.Scene.mesh_exporter = bpy.props.PointerProperty(type=MeshExporterProperties)
    bpy.utils.register_class(MeshExporterPanel)
    bpy.utils.register_class(ExportMeshData)

def unregister():
    bpy.utils.unregister_class(MeshExporterProperties)
    del bpy.types.Scene.mesh_exporter
    bpy.utils.unregister_class(MeshExporterPanel)
    bpy.utils.unregister_class(ExportMeshData)

if __name__ == "__main__":
    register()
