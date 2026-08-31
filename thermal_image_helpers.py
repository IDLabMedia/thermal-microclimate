import geopandas as gpd
import numpy as np
import rioxarray
from rasterio.enums import Resampling


def load_shp(shp_path):
    shp = gpd.read_file(shp_path)
    # Shape-file row 0 is the no-mow part and row 1 is the mow part.
    return shp


def load_thermal(path, shape, gsd_m=0.064):
    if gsd_m is not None and gsd_m <= 0:
        raise ValueError("gsd_m must be positive or None")

    thermal = rioxarray.open_rasterio(path)

    # The first band contains thermal data. 65534 and 65535 are no-data values.
    thermal = thermal[0].astype(np.float32)
    thermal = thermal.rio.write_nodata(np.nan)
    thermal = thermal.where(thermal != 65534)
    thermal = thermal.where(thermal != 65535)
    # Convert to degrees Celsius.
    thermal = thermal * (100 / 65535) - 20.15

    if gsd_m is None:
        thermal = thermal.rio.reproject("EPSG:31370")
    else:
        thermal = thermal.rio.reproject(
            "EPSG:31370",
            resolution=gsd_m,
            resampling=Resampling.average,
        )

    thermal = thermal.rio.clip(geometries=[shape], drop=True, invert=False)
    return thermal


def clip_zones(data, shp, edge_size=1, mow_no_mow_buffer=0.2):
    """
    Clip thermal data to the no-mow, mow, and shared-edge zones.

    mow_no_mow_buffer keeps pixels away from the shape boundary to avoid
    overlapping pixels and compensate for edge inaccuracies.
    """
    nomow = data.rio.clip(
        geometries=[shp.geometry[0].buffer(-mow_no_mow_buffer)], drop=False
    )
    mow = data.rio.clip(
        geometries=[shp.geometry[1].buffer(-mow_no_mow_buffer)], drop=False
    )
    edge_geom = (
        shp.geometry[0]
        .buffer(edge_size / 2)
        .intersection(shp.geometry[1].buffer(edge_size / 2))
    )
    edge = data.rio.clip(geometries=[edge_geom], drop=False)
    return nomow, mow, edge


# Sliding window filter which tumbles, meaning it jumps with the same size
# as the window and therefore produces non-overlapping windows.
def tumbling_filter(data, window_size, func):
    if isinstance(window_size, bool) or not isinstance(
        window_size, (int, np.integer)
    ):
        raise ValueError("window_size must be a positive integer")

    window_size = int(window_size)
    if window_size <= 0:
        raise ValueError("window_size must be a positive integer")

    sx, sy = data.shape
    if window_size > sx or window_size > sy:
        raise ValueError("window_size must not exceed either data dimension")

    return [
        [
            func(data[x : x + window_size, y : y + window_size].data)
            for y in range(0, sy - window_size + 1, window_size)
        ]
        for x in range(0, sx - window_size + 1, window_size)
    ]
