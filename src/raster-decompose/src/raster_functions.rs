use std::fs::File;
use std::path::Path;
use tiff::decoder::{Decoder, DecodingResult};
use ndarray::{Array2, s};
use thiserror::Error;
use linfa::traits::{Fit, Transformer};
use linfa::DatasetBase;
use linfa_reduction::Pca;
use csv::Writer;


#[derive(Error, Debug)]
pub enum TiffReadError {
    #[error("TIFF decoding error: {0}")]
    DecodeError(#[from] tiff::TiffError),

    #[error("Shape mismatch or conversion error: {0}")]
    ShapeError(#[from] ndarray::ShapeError),

    #[error("Unsupported pixel format in TIFF")]
    UnsupportedPixelFormat,

    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),
}

pub fn read_bands<P: AsRef<Path>>(path: P) -> Result<Vec<Array2<f64>>, TiffReadError> {
    let file = File::open(path)?;
    let mut decoder = Decoder::new(file)?;
    let mut bands = Vec::new();

    loop {
        let (width, height) = decoder.dimensions()?;
        let band: Array2<f64> = match decoder.read_image()? {
            DecodingResult::U8(buf) => {
                Array2::from_shape_vec((height as usize, width as usize), buf.into_iter().map(|v| v as f64).collect())?
            }
            DecodingResult::U16(buf) => {
                Array2::from_shape_vec((height as usize, width as usize), buf.into_iter().map(|v| v as f64).collect())?
            }
            DecodingResult::F32(buf) => {
                Array2::from_shape_vec((height as usize, width as usize), buf.into_iter().map(|v| v as f64).collect())?
            }
            _ => return Err(TiffReadError::UnsupportedPixelFormat),
        };
        bands.push(band);

        let has_more = decoder.more_images();
        if has_more {
            decoder.next_image()?; 
        } else {
            break;
        }
    }

    Ok(bands)
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

pub fn write_csv(data: &Array2<f64>, path: &str) -> Result<(), Box<dyn std::error::Error>> {
    let mut wtr = Writer::from_path(path)?;
    for row in data.rows() {
        wtr.serialize(row.to_vec())?;
    }
    wtr.flush()?;
    Ok(())
}
