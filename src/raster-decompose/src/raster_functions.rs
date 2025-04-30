// Rust raster utilities using GDAL for maximum compatibility
use std::path::Path;
use gdal::{Dataset, Metadata};
use ndarray::{Array2, s};
use thiserror::Error;
use linfa::traits::{Fit, Transformer};
use linfa::DatasetBase;
use linfa_reduction::Pca;
use csv::Writer;

#[derive(Error, Debug)]
pub enum TiffReadError {
    #[error("Error reading the raster file")]
    GdalReadError(#[from] gdal::errors::GdalError),

    #[error("Shape error: {0}")]
    ShapeError(#[from] ndarray::ShapeError),
}

/// Struct to hold band data and names
pub struct BandData {
    pub bands: Vec<Array2<f64>>,
    pub names: Vec<String>,
}

pub fn read_bands<P: AsRef<Path>>(path: P) -> Result<BandData, TiffReadError> {
    let dataset = Dataset::open(path)?;
    let size = dataset.raster_size();

    let mut bands = Vec::new();
    let mut names = Vec::new();

    for i in 1..=dataset.raster_count() {
        let band = dataset.rasterband(i)?;
        let buffer = band.read_as::<f32>((0, 0), size, size, None)?;
        let (width, height) = size;

        // Band name: use description if available, else "Band {i}"
        let name = band.description().unwrap_or_else(|_| format!("Band_{}", i));
        names.push(name);
        

        // Convert flat buffer to 2D Array2<f64>
        let data: Array2<f64> = Array2::from_shape_fn((height, width), |(y, x)| {
            buffer.data()[y * width + x] as f64
        });

        bands.push(data);
    }

    Ok(BandData { bands, names })
}

pub fn normalize_band(band: &Array2<f64>) -> Array2<f64> {
    let mean = band.mean().unwrap();
    let std = band.std(0.0);
    (band - mean) / std
}

pub fn stack_bands(bands: &[Array2<f64>]) -> Array2<f64> {
    let (h, w) = bands[0].dim();
    let n = h * w;
    let mut out = Array2::<f64>::zeros((n, bands.len()));
    for (i, b) in bands.iter().enumerate() {
        out.column_mut(i).assign(&b.clone().into_shape(n).unwrap());
    }
    out
}

pub fn compute_pca(data: &Array2<f64>, n_components: usize) -> Result<Array2<f64>, Box<dyn std::error::Error>> {
    let dataset = DatasetBase::new(data.clone(), ());
    let pca_model = Pca::params(n_components).fit(&dataset)?;
    let transformed = pca_model.transform(dataset);
    Ok(transformed.records)
}

pub fn local_mean(band: &Array2<f64>, k: usize) -> Array2<f64> {
    let (h, w) = band.dim();
    let mut out = Array2::<f64>::zeros((h, w));
    let offset = k / 2;
    for y in offset..(h - offset) {
        for x in offset..(w - offset) {
            let window = band.slice(s![y - offset..=y + offset, x - offset..=x + offset]);
            out[[y, x]] = window.mean().unwrap_or(0.0);
        }
    }
    out
}

pub fn write_csv(data: &Array2<f64>, headers: &[String], path: &str) -> Result<(), Box<dyn std::error::Error>> {
    let mut wtr = Writer::from_path(path)?;

    // Write headers
    wtr.write_record(headers)?;

    // Write rows
    for row in data.rows() {
        let record: Vec<String> = row.iter().map(|v| v.to_string()).collect();
        wtr.write_record(&record)?;
    }

    wtr.flush()?;
    Ok(())
}
