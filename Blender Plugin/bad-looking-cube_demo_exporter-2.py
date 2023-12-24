import bpy
import csv
import os
import math
import re

bl_info = {
    "name": "Bad Looking Cube B3D Collection Exporter",
    "author": "Krzysztof Krystian Jankowski",
    "version": (2, 2),
    "blender": (2, 80, 0),
    "location": "View3D > Sidebar > Bad Looking Cube Tab",
    "description": "Exports mesh data to a CSV file",
    "warning": "",
    "wiki_url": "",
    "category": "Import-Export",
}



class MeshExporterProperties(bpy.types.PropertyGroup):
    # DEMO end time
    demo_end_time: bpy.props.FloatProperty(name="Demo End Time")

    # SKYBOX model selection
    skybox_model: bpy.props.PointerProperty(
        name="Skybox Model",
        type=bpy.types.Object,
        description="Select Skybox model from the scene")

    # TERRAIN model selection
    terrain_model: bpy.props.PointerProperty(
        name="Terrain Model",
        type=bpy.types.Object,
        description="Select Terrain model from the scene")

    # Sun and Ambient Colors
    sun_color: bpy.props.FloatVectorProperty(
        name="Sun Color",
        subtype='COLOR',
        default=(1.0, 1.0, 1.0),
        min=0.0,
        max=1.0)

    ambient_color: bpy.props.FloatVectorProperty(
        name="Ambient Color",
        subtype='COLOR',
        default=(1.0, 1.0, 1.0),
        min=0.0,
        max=1.0)

    # Fog Settings
    fog_enable: bpy.props.BoolProperty(name="Enable Fog")
    fog_color: bpy.props.FloatVectorProperty(
        name="Fog Color",
        subtype='COLOR',
        default=(0.5, 0.5, 0.5),
        min=0.0,
        max=1.0)
    fog_density: bpy.props.FloatProperty(name="Fog Density", default=0.003, min=0.0)

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
            
        # DEMO end time
        layout.prop(mesh_exporter, "demo_end_time")

        # Skybox and Terrain Model Selectors
        layout.prop(mesh_exporter, "skybox_model")
        layout.prop(mesh_exporter, "terrain_model")

        # Sun and Ambient Color
        layout.prop(mesh_exporter, "sun_color", text="Sun Color")
        layout.prop(mesh_exporter, "ambient_color", text="Ambient Color")

        # Fog Settings
        layout.prop(mesh_exporter, "fog_enable", text="Enable Fog")
        if mesh_exporter.fog_enable:
            layout.prop(mesh_exporter, "fog_color", text="Fog Color")
            layout.prop(mesh_exporter, "fog_density", text="Fog Density")
        
        
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
                    
                demo_end_time = mesh_exporter.demo_end_time
                skybox_model = mesh_exporter.skybox_model
                terrain_model = mesh_exporter.terrain_model
                sun_color = mesh_exporter.sun_color
                ambient_color = mesh_exporter.ambient_color
                fog_enable = mesh_exporter.fog_enable
                fog_color = mesh_exporter.fog_color
                fog_density = mesh_exporter.fog_density
                
                skybox_name = self.clean_name(skybox_model.name if skybox_model else "None")
                terrain_name = self.clean_name(terrain_model.name if terrain_model else "None")
                
                writer.writerow(['-1', 'start', 'demo', 0,0,0,0,0,0,0])
                
                writer.writerow(['-1', 'skybox', skybox_name,   0,0,0,0,0,0,0])
                writer.writerow(['-1', 'terrain', terrain_name, 0,0,0,0,0,0,0])
                
                if fog_enable:
                    writer.writerow(['-1', 'fog', 'enable',fog_density,0,0,fog_color.r,fog_color.g,fog_color.b,0])
                
                writer.writerow(['-1', 'sun', '',0,0,0,sun_color.r,sun_color.g,sun_color.b,0])
                writer.writerow(['-1', 'ambient', '',0,0,0,ambient_color.r,ambient_color.g,ambient_color.b,0])

                collection = bpy.context.collection  # for example, the current context collection
                sorted_objects = sorted(collection.all_objects, key=lambda obj: obj.name)

                last_object_name = ''
                
                for obj in sorted_objects:
                   
                    if obj.type == 'MESH':
                        loc = obj.location
                        rot = obj.rotation_euler
                        rot_deg = [math.degrees(angle) % 360 for angle in rot]
                        scale = obj.scale.x                        
                        cleaned_name = self.clean_name(obj.name)
                        mode = 'clone'
                        if last_object_name != cleaned_name:
                            last_object_name = cleaned_name
                            mode = 'load'
                        
                        writer.writerow(['-1', mode, cleaned_name, -loc.x, loc.z, loc.y, rot_deg[0], rot_deg[2], rot_deg[1], scale])
                
                for obj in collection.all_objects: 
                    if obj.animation_data and obj.animation_data.action:
                        # Extract keyframes for this object
                        keyframes = set()
                        interpolation_modes = {}
                        
                        for fcurve in obj.animation_data.action.fcurves:
                            for keyframe in fcurve.keyframe_points:
                                frame = int(keyframe.co.x)
                                keyframes.add(frame)  # co.x is the frame number
                                if frame not in interpolation_modes or interpolation_modes[frame] != 'BEZIER':
                                    interpolation_modes[frame] = keyframe.interpolation

                                
                        for frame in sorted(keyframes):
                            bpy.context.scene.frame_set(int(frame))  # Set the scene to the frame

                            # Convert frame to time
                            time = frame / bpy.context.scene.render.fps

                            # Get object's location and rotation in Euler angles
                            loc = obj.location
                            rot = obj.rotation_euler  # Assuming the object uses Euler rotation
                            rot_deg = [math.degrees(angle) % 360 for angle in rot]
                            scale = obj.scale.x
                            cleaned_name = self.clean_name(obj.name)
                            mode = interpolation_modes[frame]
                            mode_name = 'pos' if mode == 'BEZIER' else 'pos_'
                            
                            writer.writerow([time, mode_name, cleaned_name, -loc.x, loc.z, loc.y, rot_deg[0], rot_deg[2], rot_deg[1], scale])

                writer.writerow([demo_end_time, 'end', 'demo', 0,0,0,0,0,0,0])
        
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
